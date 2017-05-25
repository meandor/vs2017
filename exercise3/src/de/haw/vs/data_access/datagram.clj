(ns de.haw.vs.data-access.datagram
  (:import (java.util Arrays)))

(defn left-fill [size ^bytes array]
  (let [padding (- size (alength array))]
    (if (>= padding 0)
      (concat (byte-array padding) array)
      (drop (- padding) array))))

(defn timestamp->bytes [n]
  (left-fill 8 (.toByteArray (biginteger n))))

(defn bytes->timestamp [^bytes raw]
  (biginteger raw))

(defn payload->bytes [s]
  (byte-array 24 (map byte s)))

(defn station->byte [station]
  (if (= "A" station)
    (byte 0x0)
    (byte 0x1)))

(defn byte->station [raw]
  (if (= (byte 0x0) raw)
    "A"
    "B"))

(defn bytes->payload [^bytes raw]
  (apply str (map char (filter #(not= 0x0 %) raw))))

(defn byte-array-empty? [^bytes bytes]
  (if (not (nil? bytes))
    (= (byte 0x0) (reduce bit-or bytes))
    true))

(defn datagram->message [^bytes datagram]
  (when (not (byte-array-empty? datagram))
    {:station-class   (byte->station (first datagram))
     :station-name    (bytes->payload (Arrays/copyOfRange datagram 1 11))
     :payload-content (bytes->payload (Arrays/copyOfRange datagram 11 25))
     :payload         (bytes->payload (Arrays/copyOfRange datagram 1 25))
     :slot            (nth datagram 25)
     :send-time       (bytes->timestamp (Arrays/copyOfRange datagram 26 34))}))

(defn message->datagram [message]
  (concat [(station->byte (:station-class message))]
          (payload->bytes (str (:station-name message) (:payload-content message)))
          [(byte (:slot message))]
          (timestamp->bytes (:send-time message))))
