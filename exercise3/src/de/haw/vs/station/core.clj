(ns de.haw.vs.station.core
  (:require [metrics.jvm.core :as jvm]
            [clojure.tools.logging :as log]
            [de.otto.tesla.system :as system]
            [de.otto.tesla.serving-with-httpkit :as httpkit])
  (:gen-class))

(defn station-system [runtime-config]
  (-> (system/base-system (merge {:name "exercise3"} runtime-config))
      #_(assoc
          :product-data-reader (c/using (pdr/new-product-data-reader) [:config :app-status])
                               :outfit-importer (c/using (oi/new-outfit-importer) [:config :app-status]))
      (httpkit/add-server)))

(defn -main [& args]
  (jvm/instrument-jvm)
  (Thread/setDefaultUncaughtExceptionHandler
    (reify Thread$UncaughtExceptionHandler
      (uncaughtException [_ thread ex]
        (log/error ex "Uncaught exception on " (.getName thread)))))
  (system/start (station-system {})))
