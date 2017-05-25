(ns de.haw.vs.business-logic.station
  (:require [com.stuartsierra.component :as c]
            [clojure.tools.logging :as log]
            [de.haw.vs.data-access.connector :as con]
            [de.otto.tesla.stateful.app-status :as appstat]
            [de.otto.status :as stat])
  (:import (java.net MulticastSocket)))

(defn find-free-slots [slots messages]
  (filter (fn [slot] (nil? (some #{slot} (map :slot messages)))) (drop 1 (range (+ 1 slots)))))

(defn read-messages [connector duration slots]
  (->> (range slots)
       (map (fn [& _] (con/read-message connector (/ duration slots))))
       (filter #(not (nil? %)))))

(defn select-free-slot! [state-atom duration slots connector]
  (->> (read-messages connector duration slots)
       (find-free-slots slots)
       (first)
       (swap! state-atom assoc :slot)))

(defn status [slot-atom]
  (if (nil? @slot-atom)
    (stat/status-detail :station :error "No slot assigned")
    (stat/status-detail :station :ok @slot-atom)))

(defrecord Station [config app-status connector]
  c/Lifecycle
  (start [self]
    (log/info "-> starting Station Component")
    (let [config-params (:config config)
          state-atom (atom {:slot          nil
                            :station-class (:station-class config-params)
                            :utc-offset    (:utc-offset config-params)})
          new-self (assoc self :slot state-atom)]
      (select-free-slot! state-atom (:frame-size config-params) (:slots-count config-params) connector)
      (appstat/register-status-fun app-status #(status state-atom))
      new-self))

  (stop [self]
    (log/info "<- stopping Station Component")
    self))

(defn new-station []
  (map->Station {}))
