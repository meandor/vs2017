(ns de.haw.vs.station.core
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

(defn -main [& args]
  (jvm/instrument-jvm)
  (prn args)
  (Thread/setDefaultUncaughtExceptionHandler
    (reify Thread$UncaughtExceptionHandler
      (uncaughtException [_ thread ex]
        (log/error ex "Uncaught exception on " (.getName thread)))))
  (system/start (station-system {})))
