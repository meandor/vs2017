(ns de.haw.vs.business-logic.station
  (:require [com.stuartsierra.component :as c]
            [clojure.tools.logging :as log])
  (:import (java.net MulticastSocket)))

;(defn read [duration slots receiver]
;  (.setSoTimeout (:socket @(:socket-connection receiver)) (/ duration slots))
;  (->> (range slots)
;       (map #(r/read-message receiver))
;       (filter #(not (nil? %)))))

(defrecord Station [config app-status receiver]
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
