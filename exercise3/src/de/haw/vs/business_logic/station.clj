(ns de.haw.vs.business-logic.station
  (:require [com.stuartsierra.component :as c]
            [clojure.tools.logging :as log]))

(defrecord Station [config app-status connector]
  c/Lifecycle
  (start [self]
    (log/info "-> starting Station Component")
    (let [config-params (:config config)])
    self)

  (stop [self]
    (log/info "<- stopping Station Component")
    self))

(defn new-station []
  (map->Station {}))
