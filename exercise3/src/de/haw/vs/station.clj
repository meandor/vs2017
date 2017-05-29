(ns de.haw.vs.station
  (:require [com.stuartsierra.component :as c]
            [clojure.tools.logging :as log]
            [clojure.core.async :as async]
            [de.haw.vs.networking.connector :as con]
            [de.haw.vs.clock :as clk])
  (:import (java.net MulticastSocket)))

(defn collection-not-empty-or-nil
  "Return the collection if it is not empty, otherwise nil"
  [col]
  (when (> (count col) 0)
    col))

(defn find-free-slots [slots messages]
  (->> (+ 1 slots)
       (range)
       (drop 1)
       (filter (fn [slot] (nil? (some #{slot} (map :slot messages)))))))

(defn read-messages [connector duration slots]
  (->> (range slots)
       (map (fn [& _] (con/read-message-with-collision-detection connector (/ duration slots))))
       (filter #(not (nil? %)))))

(defn put-message-on-channel [channel messages]
  (doseq [message messages]
    (async/go (async/>! channel message)))
  messages)

(defn add-station-params-to-message [state-atom payload-map]
  (assoc payload-map
    :slot (:slot @state-atom)
    :station-class (:station-class @state-atom)))

(defn send-phase
  "Sends a message from the input channel if any is present"
  [state-atom input-chan connector]
  (if (< 0 (.count (.buf input-chan)))
    (->> (async/<!! input-chan)
         (add-station-params-to-message state-atom)
         (con/send-message-collision-safe? connector))
    false))

(defn- log-slot [slot]
  (log/info "found free slot: " slot)
  slot)

(defn read-phase
  "Reads messages from the socket, finds free slots and takes random slot from
   that as new slot to send on. The read messages are put on the writer channel."
  [duration slots output-channel connector]
  (when (and (< 0 slots) (< 0 duration))
    (some->> (read-messages connector duration slots)
             (put-message-on-channel output-channel)
             (find-free-slots slots)
             (collection-not-empty-or-nil)
             (rand-nth)
             (log-slot))))

(defn main-phase
  "Read until chosen slot, choose new slot for next frame, send message on that slot,
   use the new slot, read until the end of frame and do everything again."
  [state-atom duration slots in-chan out-chan connector]
  (let [duration-per-slot (/ duration slots)
        before-slots (- (:slot @state-atom) 1)
        after-slots (- (+ slots 1) (:slot @state-atom))]
    (some->> (read-phase (* duration-per-slot before-slots) before-slots out-chan connector)
             (swap! state-atom assoc :slot))
    (Thread/sleep (/ duration-per-slot 2))
    (when (send-phase state-atom in-chan connector)         ; when collision is detected, signal need of new slot number
      (swap! state-atom assoc :slot nil))
    (if (nil? (:slot @state-atom))                          ; read all messages after the slot, assign new slot if send was unsuccessful before
      (some->> (read-phase (* duration-per-slot after-slots) after-slots out-chan connector)
               (swap! state-atom assoc :slot))
      (read-phase (* duration-per-slot after-slots) after-slots out-chan connector)))
  (main-phase state-atom duration slots in-chan out-chan connector))

(defn initial-phase
  "Read until frame end. Read phase function finds free slot to send on."
  [state-atom duration slots out-chan connector]
  (let [remaining-slots (clk/remaining-slots duration slots)
        remaining-duration (* remaining-slots (/ duration slots))]
    (some->> (read-phase remaining-duration remaining-slots out-chan connector)
             (swap! state-atom assoc :slot))))

(defrecord Station [config app-status connector message-writer in-chan out-chan]
  c/Lifecycle
  (start [self]
    (log/info "-> starting Station Component")
    (let [{:keys [station-class utc-offset frame-size slots-count]} (:config config)
          state-atom (atom {:slot          nil
                            :station-class station-class})
          new-self (assoc self :slot state-atom)]
      (reset! clk/offset utc-offset)                        ; set initial utc offset

      (async/thread (initial-phase state-atom frame-size slots-count out-chan connector)
                    (main-phase state-atom frame-size slots-count in-chan out-chan connector))
      new-self))

  (stop [self]
    (log/info "<- stopping Station Component")
    self))

(defn new-station [in-chan out-chan]
  (map->Station {:in-chan  in-chan
                 :out-chan out-chan}))
