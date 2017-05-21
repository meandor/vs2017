(defproject exercise3 "0.1.0-SNAPSHOT"
  :description "Defines a Station that is sending and receiving messages on a multicast socket via stdma."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [de.otto/tesla-microservice "0.9.2"]]
  :main ^:skip-aot exercise3.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
