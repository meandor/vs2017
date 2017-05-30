(ns de.haw.vs.station
  (:require [com.stuartsierra.component :as c]
            [clojure.tools.logging :as log]
            [clojure.core.async :as async]
            [de.haw.vs.networking.connector :as con]
            [de.haw.vs.clock :as clk])
  (:import (java.net MulticastSocket)))

(defn range-starting-with-one [last]
  (->> (+ 1 last)
       (range)
       (drop 1)))

(defn find-free-slots [available-slot-list messages]
  (filter (fn [slot] (nil? (some #{slot} (map :slot messages)))) available-slot-list))

(defn read-messages [connector duration slots]
  (->> (range slots)
       (map (fn [& _] (con/read-message-with-collision-detection connector (/ duration slots))))
       (filter #(not (nil? %)))))

(defn put-messages-on-channel [channel messages]
  (doseq [message messages]
    (async/go (async/>! channel message)))
  messages)

(defn add-station-params-to-message [state-atom payload-map]
  (assoc payload-map
    :slot (:slot @state-atom)
    :station-class (:station-class @state-atom)))

(defn message-was-send?
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

(defn save-into-atom [atom value]
  (reset! atom value)
  value)

(defn main-phase
  "Read until chosen slot, choose new slot for next frame, send message on that slot,
   use the new slot, read until the end of frame and do everything again."
  [state-atom duration slots in-chan out-chan connector]
  (let [duration-per-slot (/ duration slots)
        before-slots (- (:slot @state-atom) 1)
        after-slots (- (+ slots 1) (:slot @state-atom))
        free-slots (atom [])]
    #_(log/info "current slot: " (:slot @state-atom))
    (some->> (read-messages connector (* before-slots duration-per-slot) before-slots) ; read until chosen slot and select slot for next frame
             (find-free-slots (range-starting-with-one slots))
             (save-into-atom free-slots)
             (rand-nth)
             (log-slot)
             (swap! state-atom assoc :slot))
    (if (message-was-send? state-atom in-chan connector)
      (read-messages connector (* after-slots duration-per-slot) after-slots) ; collision free sending
      (some->> (read-messages connector (* after-slots duration-per-slot) after-slots) ;if collision during sending, take a new slot
               (find-free-slots @free-slots)
               (rand-nth)
               (log-slot)
               (swap! state-atom assoc :slot))))
  (main-phase state-atom duration slots in-chan out-chan connector))

(defn initial-phase
  "Read until frame end, find free slots, take a random one, use it for sending."
  [state-atom duration slots out-chan connector]
  (let [remaining-slots (clk/remaining-slots duration slots)
        remaining-duration (* remaining-slots (/ duration slots))]
    (some->> (read-messages connector remaining-duration remaining-slots)
             (put-messages-on-channel out-chan)
             (find-free-slots (range-starting-with-one slots))
             (rand-nth)
             (log-slot)
             (swap! state-atom assoc :slot))))

(defrecord Station [config app-status connector message-writer in-chan out-chan]
  c/Lifecycle
  (start [self]
    (log/info "-> starting Station Component")
    (let [{:keys [station-class utc-offset frame-size slots-count]} (:config config)
          state-atom (atom {:slot          (rand-int slots-count)
                            :station-class station-class})
          new-self (assoc self :slot state-atom)]
      (reset! clk/offset utc-offset)                        ; set initial utc offset
      (async/thread (initial-phase state-atom frame-size slots-count out-chan connector)
                    (clk/wait-until-slot-end (/ frame-size slots-count))
                    (log/info "Starting main phase")
                    (main-phase state-atom frame-size slots-count in-chan out-chan connector))
      new-self))

  (stop [self]
    (log/info "<- stopping Station Component")
    self))

(defn new-station [in-chan out-chan]
  (map->Station {:in-chan  in-chan
                 :out-chan out-chan}))
