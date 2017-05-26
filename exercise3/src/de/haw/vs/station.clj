(ns de.haw.vs.station
  (:require [com.stuartsierra.component :as c]
            [clojure.tools.logging :as log]
            [de.haw.vs.networking.connector :as con]
            [de.otto.tesla.stateful.app-status :as appstat]
            [de.otto.status :as stat]
            [clojure.core.async :as async])
  (:import (java.net MulticastSocket)))

(defn current-time [offset]
  (+ (System/currentTimeMillis) offset))

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

(defn build-message [state-atom payload-map]
  (assoc payload-map
    :slot (:slot @state-atom)
    :station-class (:station-class @state-atom)
    :send-time (current-time (:utc-offset @state-atom))))

(defn send-phase!
  "Sends a message from the input channel if any is present"
  [state-atom input-chan connector]
  (when (< 0 (.count (.buf input-chan)))
    (->> (async/<!! input-chan)
         (build-message state-atom)
         (con/send-message connector))))

(defn- log-slot [slot]
  (log/info "found free slot: " slot)
  slot)

(defn read-phase!
  "Reads messages from the socket and tries to find an empty slot to send on.
   The read messages are put on the writer channel. Switch to send-phase afterwards."
  [state-atom duration slots output-channel connector]
  (when (and (< 0 slots) (< 0 duration))
    (->> (read-messages connector duration slots)
         (put-message-on-channel! output-channel)
         (find-free-slots slots)
         (first)
         (log-slot)
         (swap! state-atom assoc :slot))))

(defn run-phases!
  "Sends at the chosen slot and receive before and after that slot"
  [state-atom duration slots in-chan out-chan connector]
  (let [duration-per-slot (/ duration slots)
        before-slots (- (:slot @state-atom) 1)
        after-slots (- (+ slots 1) (:slot @state-atom))]
    (con/attach-client-socket connector)                    ; Read all messages before the slot
    (read-phase! state-atom (* duration-per-slot before-slots) before-slots out-chan connector)
    (con/attach-server-socket connector)
    (Thread/sleep (/ duration-per-slot 2))                  ; send in the middle of my slot
    (send-phase! state-atom in-chan connector)
    (con/attach-client-socket connector)
    (read-phase! state-atom (* duration-per-slot after-slots) after-slots out-chan connector))
  (run-phases! state-atom duration slots in-chan out-chan connector))

(defn status [state-atom]
  (if (nil? (:slot @state-atom))
    (stat/status-detail :station :error "No slot assigned")
    (stat/status-detail :station :ok @state-atom)))

(defrecord Station [config app-status connector message-writer in-chan out-chan]
  c/Lifecycle
  (start [self]
    (log/info "-> starting Station Component")
    (let [{:keys [station-class utc-offset frame-size slots-count]} (:config config)
          state-atom (atom {:slot          nil
                            :station-class station-class
                            :utc-offset    utc-offset})
          new-self (assoc self :slot state-atom)]
      (async/thread (read-phase! state-atom frame-size slots-count out-chan connector) ; initial discovery for full frame size
                    (run-phases! state-atom (- frame-size (/ frame-size slots-count)) (- slots-count 1) in-chan out-chan connector)) ; after that run for one station less
      (appstat/register-status-fun app-status #(status state-atom))
      new-self))

  (stop [self]
    (log/info "<- stopping Station Component")
    self))

(defn new-station [in-chan out-chan]
  (map->Station {:in-chan  in-chan
                 :out-chan out-chan}))
