(ns de.haw.vs.data-links.payload-source
  (:require [clojure.tools.logging :as log]
            [com.stuartsierra.component :as c]
            [clojure.core.async :as async]
            [clojure.java.io :as io])
  (:import (java.io Reader)
           (java.nio CharBuffer)))

(defn char-array->payload-message [char-array]
  {:station-name    (reduce str (take 10 char-array))
   :payload         (reduce str char-array)
   :payload-content (reduce str (drop 10 char-array))})

(defn send-messages-to-channel [payload-size input out-chan]
  (with-open [r (io/reader input)]
    (loop [buffer (char-array payload-size)
           read-bytes (.read r buffer 0 (count buffer))]
      (if (not= read-bytes -1)
        (let [new-buffer (char-array payload-size)]
          (log/debug "read payload: " (char-array->payload-message buffer))
          (async/go (async/>! out-chan (char-array->payload-message buffer)))
          (recur new-buffer (.read r new-buffer 0 (count new-buffer))))))))

(defrecord PayloadSource [config out-chan]
  c/Lifecycle
  (start [self]
    (log/info "-> starting PayloadSource Component")
    (async/thread (send-messages-to-channel (get-in config [:config :payload-size]) *in* out-chan))
    self)

  (stop [self]
    (log/info "<- stopping PayloadSource Component")
    self))

(defn new-payload-source [out-chan]
  (map->PayloadSource {:out-chan out-chan}))
