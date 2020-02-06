(defproject wtd.server "0.1.0-SNAPSHOT"
  :description "Server providing RPC API for use by wtd.el and other potential wtd clients"
  :url "https://github.com/senters/wtd"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/tools.logging "0.4.1"]
                 [clj-http "3.9.1"]
                 [compojure "1.6.1"]
                 [cheshire "5.8.1"]
                 [cider/piggieback "0.3.8"]
                 [hiccup "1.0.5"]
                 [http-kit "2.2.0"]
                 [ring "1.5.1"]
                 [ring/ring-defaults "0.3.2"]]
  :plugins [[lein-ring "0.12.5"]]
  :repl-options {:init-ns wtd.server}
  :ring {:handler wtd.server/app}
  :main wtd.server)
