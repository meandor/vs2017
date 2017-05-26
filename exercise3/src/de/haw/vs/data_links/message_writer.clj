(ns de.haw.vs.data-links.message-writer
  (:require [com.stuartsierra.component :as c]
            [clojure.tools.logging :as log]
            [clj-time.coerce :as co]
            [clj-time.format :as fo]
            [clojure.core.async :as async]))

(defn message->text [message]
  (let [date-string (fo/unparse (fo/formatter :date-hour-minute-second-ms) (co/from-long (:send-time message)))]
    (format "%s %s (%s): %s\n" date-string (:station-name message) (:station-class message) (:payload-content message))))

(defn append-to-file [filename message]
  (spit filename (message->text message) :append true))

(defn append-messages-to-file [file channel]
  (async/go
    (while true
      (append-to-file file (async/<! channel)))))

(defrecord MessageWriter [config in-chan]
  c/Lifecycle
  (start [self]
    (log/info "-> starting MessageWriter Component")
    (append-messages-to-file (get-in config [:config :output-file]) in-chan)
    self)

  (stop [self]
    (log/info "<- stopping MessageWriter Component")
    self))

(defn new-message-writer [in-chan]
  (map->MessageWriter {:in-chan in-chan}))
