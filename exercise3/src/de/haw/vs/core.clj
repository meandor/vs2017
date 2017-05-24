(ns de.haw.vs.core
  (:require [metrics.jvm.core :as jvm]
            [clojure.tools.logging :as log]
            [de.otto.tesla.system :as system]
            [de.otto.tesla.serving-with-httpkit :as httpkit]
            [com.stuartsierra.component :as c]
            [de.haw.vs.data-access.connector :as con])
  (:gen-class))

(defn station-system [runtime-config]
  (-> (system/base-system (merge {:name "exercise3"} runtime-config))
      (assoc
        :connector (c/using (con/new-connector) [:config :app-status]))
      (httpkit/add-server)))

(defn display-help []
  (println "Execute: java -jar exercise3.jar INTERFACENAME MULTICASTADDRESS PORT STATIONCLASS UTCOFFSET\n"))

(defn -main [& args]
  (jvm/instrument-jvm)
  (Thread/setDefaultUncaughtExceptionHandler
    (reify Thread$UncaughtExceptionHandler
      (uncaughtException [_ thread ex]
        (log/error ex "Uncaught exception on " (.getName thread)))))
  (if (= 5 (count args))
    (system/start (station-system {:interface-name    (first args)
                                   :multicast-address (second args)
                                   :socket-port       (nth args 3)
                                   :station-class     (nth args 4)
                                   :utc-offset        (nth args 5)}))
    (display-help)))
