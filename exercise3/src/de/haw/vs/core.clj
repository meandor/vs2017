(ns de.haw.vs.core
  (:require [metrics.jvm.core :as jvm]
            [clojure.tools.logging :as log]
            [clojure.core.async :as async]
            [de.otto.tesla.system :as system]
            [com.stuartsierra.component :as c]
            [de.haw.vs.networking.connector :as connector]
            [de.haw.vs.station :as station]
            [de.haw.vs.data-links.message-writer :as message-writer]
            [de.haw.vs.data-links.payload-source :as payload-source]
            [de.haw.vs.clock :as clock])
  (:gen-class))

(defn station-system [runtime-config]
  (let [station->message-writer (async/chan 100)
        station->clock (async/chan 100)
        payload-source->station (async/chan 100)]

    (-> (system/base-system (merge {:name "exercise3"} runtime-config))
        (assoc
          :payload-source (c/using (payload-source/new-payload-source payload-source->station) [:config])
          :message-writer (c/using (message-writer/new-message-writer station->message-writer) [:config])
          :message-writer (c/using (clock/new-clock station->clock) [:config])
          :connector (c/using (connector/new-connector) [:config])
          :station (c/using (station/new-station payload-source->station station->message-writer station->clock) [:config :connector])))))

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
