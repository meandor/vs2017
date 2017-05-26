(ns de.haw.vs.station
  (:require [com.stuartsierra.component :as c]
            [clojure.tools.logging :as log]
            [de.haw.vs.networking.connector :as con]
            [de.otto.tesla.stateful.app-status :as appstat]
            [de.otto.status :as stat]
            [clojure.core.async :as async])
  (:import (java.net MulticastSocket)))

(defn find-free-slots [slots messages]
  (filter (fn [slot] (nil? (some #{slot} (map :slot messages)))) (drop 1 (range (+ 1 slots)))))

(defn read-messages [connector duration slots]
  (->> (range slots)
       (map (fn [& _] (con/read-message connector (/ duration slots))))
       (filter #(not (nil? %)))))

(defn put-message-on-channel! [channel messages]
  (doseq [message messages]
    (async/go (async/>! channel message)))
  messages)

(defn read-phase!
  "Reads messages from the socket and tries to find an empty slot to send on.
   The read messages are put on the writer channel."
  [state-atom duration slots output-channel connector]
  (->> (read-messages connector duration slots)
       (put-message-on-channel! output-channel)
       (find-free-slots slots)
       (first)
       (swap! state-atom assoc :slot)))

(defn status [state-atom]
  (if (nil? (:slot @state-atom))
    (stat/status-detail :station :error "No slot assigned")
    (stat/status-detail :station :ok @state-atom)))

(defn send-message [state-atom config-params connector]
  ;TODO
  )

(defrecord Station [config app-status connector message-writer]
  c/Lifecycle
  (start [self]
    (log/info "-> starting Station Component")
    (let [config-params (:config config)
          state-atom (atom {:slot          nil
                            :station-class (:station-class config-params)
                            :utc-offset    (:utc-offset config-params)})
          new-self (assoc self :slot state-atom)]
      (read-phase! state-atom (:frame-size config-params) (:slots-count config-params) (:in-channel message-writer) connector)
      (send-message state-atom config-params connector)
      (appstat/register-status-fun app-status #(status state-atom))
      new-self))

  (stop [self]
    (log/info "<- stopping Station Component")
    self))

(defn new-station [in-chan out-chan]
  (map->Station {:in-chan  in-chan
                 :out-chan out-chan}))
