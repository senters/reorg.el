(defproject info.senters/reorg-server "0.1.0-SNAPSHOT"
  :description "server for reorg.el personal attention reorganization system"
  :url "http://senters.info"
  :license {:name "MIT License", :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [compojure "1.6.2"]
                 [hiccup "1.0.5"]
                 ;; some of the functionality belongs in senters library
                 ;; and will be moved there soon
                 ;; [info.senters/senters 0.1]
                 [ring/ring-defaults "0.3.2"]
                 [ring/ring-json "0.5.0"]
                 [ring/ring-core "1.9.1"]
                 [javax.servlet/servlet-api "2.5"]]
  :plugins [[lein-ring "0.12.5"]]
  :ring {:handler senters.pars.reorg.controller/handler
         :nrepl {:start? true
                 :port 4605}}
  :repl-options {:init-ns senters.pars.reorg.controller})
