(ns de.haw.vs.core
  (:require [metrics.jvm.core :as jvm]
            [clojure.tools.logging :as log]
            [clojure.core.async :as async]
            [de.otto.tesla.system :as system]
            [de.otto.tesla.serving-with-httpkit :as httpkit]
            [com.stuartsierra.component :as c]
            [de.haw.vs.networking.connector :as connector]
            [de.haw.vs.station :as station]
            [de.haw.vs.data-links.message-writer :as message-writer])
  (:gen-class))

(defn station-system [runtime-config]
  (-> (system/base-system (merge {:name "exercise3"} runtime-config))
      (assoc
        :connector (c/using (connector/new-connector) [:config :app-status])
        :message-writer (c/using (message-writer/new-message-writer (async/chan 10)) [:config :app-status])
        :station (c/using (station/new-station) [:config :app-status :connector :message-writer]))
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
                                   :socket-port       (read-string (nth args 2))
                                   :station-class     (nth args 3)
                                   :utc-offset        (read-string (nth args 4))}))
    (display-help)))
