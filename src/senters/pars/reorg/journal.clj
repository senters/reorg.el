(ns senters.pars.reorg.journal
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.walk :as walk]))

(def config
  {:local-journal-path "/home/rplevy/prj/wtd-personal/journal.org"
   ;; :local-journal-path "/tmp/test.org"
   :no-documents-before 20210308080752})

(def journal
  (atom {:documents nil}))

(defn ignore-first-line [lines]
  (if (re-matches #"^\* .*" (first lines))
    (next lines)
    lines))

(defn shortcut-if-no-changes [lines]
  (when (not= (:last-line-seen @journal)
              (first lines))
    (do (swap! journal update :last-line-seen (first lines))
        lines)))

(defn line-indent-level [line]
  (or (and line
           (let [[matched captured] (re-matches #"( *)[\+\-\*\d].*" line)]
             (when matched (count captured))))
      -1))

(defn tree? [node]
  (and (coll? node)
       (= 2 (count node))
       (coll? (second node))))

(def leaf? string?)

(defn tree-root [node]
  (if (tree? node)
    (first node)
    node))

(defn tree-children [node]
  (when (tree? node)
    (second node)))

(defn update-tree [tree update-fn]
  [(tree-root tree)
   (update-fn (tree-children tree))])

(defn leaf->tree [leaf child]
  (let [parent leaf] ; no longer leaf
    [parent [child]]))

(defn unflush-to-last-node [nodes value]
  (let [last-node (last nodes)]
    (conj (pop (vec nodes))
          (if (leaf? last-node)
            (str last-node " " (string/trim value))
            (update-tree last-node
                         #(unflush-to-last-node % value))))))

#_(unflush-to-last-node
   ["  - fooo"
    ["  - bar" ["quux"]]]
   "     baz")

(defn conj-at-level [tree n value]
  (let [nodes (tree-children tree)
        last-node (last nodes)
        last-root-indent (line-indent-level (tree-root last-node))
        line-continuing? (= -1 (line-indent-level value))]
    (cond-> tree
      line-continuing?       (update-tree #(unflush-to-last-node % value))
      (= n last-root-indent) (update-tree #(conj (vec %) value))
      (> n last-root-indent) (update-tree
                              (fn [nodes]
                                (conj (pop (vec nodes))
                                      (if (leaf? last-node)
                                        (leaf->tree last-node value)
                                        (conj-at-level last-node n value))))))))

(defn document-lines->document-trees [document-lines]
  {:full-text document-lines
   :calls (first ; the document tree is the only tree
           (tree-children
            (reduce (fn [nodes line]
                      (conj-at-level nodes
                                     (line-indent-level line)
                                     line))
                    [:root [(first document-lines)]]
                    (next document-lines))))})


(defn lines->documents [lines]
  (reduce (fn [documents line]
            (if (re-matches #"^\*\* .*" line)
              (conj documents [line])
              (conj (rest documents)
                    (conj (first documents) line))))
          nil
          lines))

(defn document-after-cutoff-date? [document]
  (let [[_ document-date] (re-matches #"\*\* <<.*-(\d{14})>>.*"
                                      (first document))]
    (<= (:no-documents-before config)
        (Long/parseLong document-date))))

(defn valid-call? [s]
  (re-matches #"(document|step|sign|instrument|center) .*" s))

(defn heading? [s] (re-matches #"^\*\* .*" s))

(defn clean-up-document [document]
  {:full-text (:full-text document)
   :calls (walk/postwalk (fn [node]
                           (if (leaf? node)
                             (if (heading? node)
                               (string/replace node #"^\*\* " "document ")
                               (string/replace node #"^ +[\+\-\*\d].? " ""))
                             node))
                         (:calls document))})

(defn filter-valid-call-subtrees* [document-tree & [valid-subtrees-continued]]
  (reduce (fn [valid-subtrees node]
            (if (valid-call? (tree-root node))
              (update-tree
               valid-subtrees
               (fn [nodes]
                 (conj (vec nodes) node)))
              (if (leaf? node)
                valid-subtrees
                (filter-valid-call-subtrees* node valid-subtrees))))
          (or valid-subtrees-continued
              [(tree-root document-tree) []])
          (tree-children document-tree)))

(defn filter-valid-call-subtrees [document-tree]
  (update document-tree :calls filter-valid-call-subtrees*))

(defn format-document-call [document-string]
  (let [[_ local-identifier title]
        (re-matches #"document <<(.*)>> (.*)" document-string)]
    {:local-identifier local-identifier
     :title title}))


(defn format-step [name-args-string]
  (let [[n & a] (string/split name-args-string #" ")]
    {:name n
     :args a}))

(defn format-sign [name-expr-string]
  (let [[matched? name expr] (re-matches #"([^\s]+)\s+(.*)" name-expr-string)]
    {:name name
     :command expr}))

(defn format-instrument-call [tree]
  (let [[_ instrument-name args] (re-matches #"instrument (.*)"
                                             (tree-root tree))
        args (tree-children tree)
        [[_ engine]] (keep (partial re-matches #"engine (.*)") args)
        steps (map second (keep (partial re-matches #"step (.*)") args))
        signs (map second (keep (partial re-matches #"sign (.*)") args))]
    (when engine
      (merge
       {:call :instrument
        :local-name instrument-name
        :engine engine}
       (when (seq steps) {:steps (map format-step steps)})
       (when (seq signs) {:signs (map format-sign signs)})))))

(defn format-senters-calls [document]
  (merge (format-document-call (tree-root (:calls document)))
         {:full-text (:full-text document)
          :calls (keep (fn [node]
                         (case (first (string/split (tree-root node) #" "))
                           "instrument" (format-instrument-call node)
                           ;; TODO: other types
                           node))
                       (tree-children (:calls document)))}))

(defn process-local-journal* [lines]
  (->> (ignore-first-line lines)
       #_shortcut-if-no-changes
       lines->documents
       (filter document-after-cutoff-date?)
       (map document-lines->document-trees)
       (map clean-up-document)
       (keep filter-valid-call-subtrees)
       (map format-senters-calls)
       #_collate-document-revisions))

(defn process-local-journal []
  (swap! journal assoc :documents
         (with-open [rdr (io/reader (:local-journal-path config))]
           (or (some->> (reduce conj [] (line-seq rdr))
                        process-local-journal*)
               (:documents @journal)))))

(defn get-documents []
  (:documents @journal))
