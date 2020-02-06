(ns wtd.server
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [cheshire.core :as json]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [hiccup.core :as hiccup]
            [ring.middleware.defaults :as ring-defaults]))

(defn api-route [path]
  (str "/wtd/api/0.1.0" path))

(defroutes site-routes
  (GET (api-route "/") request
    )
  (POST ))

(def app
  (routes
    (ring-defaults/wrap-defaults site-routes ring-defaults/site-defaults)))
