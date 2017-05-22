(ns de.haw.vs.data-access.datagram)

(defn timestamp->bytes [n]
  (byte-array 8 (.toByteArray (biginteger n))))

(defn payload->bytes [s]
  (byte-array 24 (map byte s)))

(defn station->byte [station]
  (if (= "A" station)
    (byte 0x0)
    (byte 0x1)))

(defn datagram->message [datagram])

(defn message->datagram [message]
  (concat [(station->byte (:station-class message))]
          (payload->bytes (:payload message))
          [(byte (:slot message))]
          (timestamp->bytes (:send-time message))))
