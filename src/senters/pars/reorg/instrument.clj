(ns senters.pars.reorg.instrument)

(def instruments (atom {:instruments nil}))

(defn source-instruments [documents]
  (->> (mapcat :calls documents)
       (filter map?)
       (filter #(= :instrument (:call %)))
       (swap! instruments assoc :instruments)))

(defn get-instruments []
  (:instruments @instruments))

(defn get-instrument [instrument]
  (filter #(= instrument (:local-name %))
          (:instruments @instruments)))
