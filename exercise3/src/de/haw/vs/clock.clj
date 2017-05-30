(ns de.haw.vs.clock
  (:require [com.stuartsierra.component :as c]
            [clojure.core.async :as async]
            [clojure.tools.logging :as log]))

(def offset (atom 0))

(defn current-time []
  (+ (System/currentTimeMillis) @offset))

(defn current-frame [frame-size]
  (quot (current-time) frame-size))

(defn current-slot [frame-size slot-count]
  (+ 1 (quot (- (mod (current-time) frame-size) 1) (/ frame-size slot-count))))

(defn remaining-slots [frame-size slot-count]
  (- slot-count (current-slot frame-size slot-count)))

(defn remaining-time-until-end [duration]
  (- duration (mod (current-time) duration)))

(defn wait-until-slot-end [slot-duration]
  (Thread/sleep (remaining-time-until-end slot-duration)))

(defn ms-until-slot-middle [frame-size slot-count slot-number]
  (let [duration-per-slot (/ frame-size slot-count)]
    (->> (current-slot frame-size slot-count)
         (- slot-number)                                    ; slot offset
         (* duration-per-slot)                              ; time offset for the desired slot
         (+ (- (/ duration-per-slot 2) (mod (current-time) duration-per-slot)))) ; time offset for middle of desired slot
    ))

(defn process-message [message]
  (when (= "A" (:station-class message))
    (swap! offset (fn [old-offset]
                    (/ (+ old-offset
                          (- (:received-time message) (:send-time message)))
                       2)))))

(defn process-messages [channel initial-offset]
  (async/go
    (reset! offset initial-offset)
    (while true
      (process-message (async/<! channel)))))

(defrecord Clock [config in-chan]
  c/Lifecycle
  (start [self]
    (log/info "-> starting Clock Component")
    (process-messages in-chan (get-in config [:config :utc-offset]))
    self)

  (stop [self]
    (log/info "<- stopping Clock Component")
    self))

(defn new-clock [in-chan]
  (map->Clock {:in-chan in-chan}))
