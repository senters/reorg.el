(ns senters.pars.reorg.controller
  (:require [clojure.pprint :refer [pprint]]
            [compojure.route :as route]
            [compojure.core :refer :all]
            [compojure.handler :as handler]
            [ring.middleware.json :as middleware]
            [senters.pars.reorg.instrument :as instrument]
            [senters.pars.reorg.journal :as journal]
            [senters.pars.reorg.voice :as voice]))

(defn maybe-update []
  (when (journal/process-local-journal)
    (instrument/source-instruments (journal/get-documents))))

(defroutes app-routes
  (GET "/0.1/voice" []
       (voice/voice-post-form))
  (GET "/0.1/voice-post" req
       (voice/queue-input (:query-params req))
       {:status 201})
  (POST "/0.1/voice-post" req
        (voice/queue-input (:form-params req))
        {:status 201})
  (GET "/0.1/voice-queue" []
       {:status 200
        :body {:documents (seq (voice/dequeue))}})
  (GET "/0.1/instruments" []
       (maybe-update)
       {:status 200
        :body {:instruments (instrument/get-instruments)}})
  (GET "/0.1/instruments/:instrument-name" [instrument-name]
       (maybe-update)
       {:status 200
        :body {:instrument (instrument/get-instrument instrument-name)}})
  (route/not-found "Not Found"))

(def handler
  (-> (handler/api app-routes)
      (middleware/wrap-json-body)
      (middleware/wrap-json-params)
      (middleware/wrap-json-response)))
