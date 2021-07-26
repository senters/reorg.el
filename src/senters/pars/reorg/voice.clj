(ns senters.pars.reorg.voice
  (:require [hiccup.core :refer :all]
            [senters.pars.reorg.instrument :as si]))

(def voice-documents (atom []))

(defn voice-post-form* [step-name args]
  [:form {:method "post" :action "/0.1/voice-post" :id "voice-form"}
   [:input {:type "submit" :value step-name :style "font-size: 100px;"}]
   [:input {:type "hidden" :name step-name :id step-name :value step-name}]
   (for [a args]
     [:input
      {:type "text"
       :name a
       :id a
       :value a
       :style "font-size: 100px;"
       :onfocus "this.value=''"}])])

(defn voice-post-form []
  (html
   [:html
    [:form {:method "post" :action "/0.1/voice-post" :id "voice-form"}
     [:input {:type "submit" :value "create new" :style "font-size: 100px;"}]
     [:input {:type "hidden" :name "create-new-doc" :id "create-new-doc"
              :value "create-new-doc"}]
     [:input {:type "text" :name "doc-title" :id "doc-title" :value "doc-title"
              :style "font-size: 100px;" :onfocus "this.value=''"}]
     [:input {:type "text" :name "doc-body" :id "doc-body" :value "doc-body"
              :style "font-size: 100px;" :onfocus "this.value=''"}]]
    (for [instr (si/get-instruments)
          :let [step (first (:steps instr))]] ; this is about to change
      [:div {:style "display: block;"}
       (voice-post-form* (str (:local-name instr) "." (:name step))
                         (:args step))])]))

(defn queue-input [data]
  (prn :got-here data)
  (swap! voice-documents conj data))

(defn dequeue []
  (when-let [response @voice-documents]
    (reset! voice-documents [])
    response))
