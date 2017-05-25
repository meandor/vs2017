(defproject exercise3 "0.1.0-SNAPSHOT"
  :description "Defines a Station that is sending and receiving messages on a multicast socket via stdma."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha16"]
                 [org.clojure/core.async "0.3.442"]
                 [clj-time "0.13.0"]
                 [metrics-clojure-jvm "2.9.0"]
                 [ring/ring-mock "0.3.0"]
                 [clj-http "3.5.0"]

                 [de.otto/tesla-microservice "0.9.2"]
                 [de.otto/tesla-httpkit "1.0.1"]
                 [de.otto/tesla-basic-logging "0.1.5"]]
  :main ^:skip-aot de.haw.vs.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev     {:dependencies [[org.clojure/test.check "0.9.0"]]}})
