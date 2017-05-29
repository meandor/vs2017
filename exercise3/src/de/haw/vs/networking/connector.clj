(ns de.haw.vs.networking.connector
  (:require [clojure.tools.logging :as log]
            [com.stuartsierra.component :as c]
            [de.haw.vs.networking.datagram :as dg]
            [de.haw.vs.clock :as clk])
  (:import (java.net InetAddress NetworkInterface MulticastSocket DatagramPacket DatagramSocket SocketTimeoutException)))

(defn socket-atom [interface-name address port]
  (atom {:socket nil :received-messages 0 :send-messages 0 :address address :interface interface-name :port port}))

(defn- read-bytes-from-socket [^MulticastSocket socket ^DatagramPacket packet]
  (try
    (.receive socket packet)
    (log/info "Got message")
    (catch SocketTimeoutException e
      (log/info "Did not get any message"))))

(defn- read-message [socket-connection datagram-bytes timeout]
  (.setSoTimeout (:socket @socket-connection) timeout)
  (let [buffer (byte-array datagram-bytes)
        packet (new DatagramPacket buffer (count buffer))]
    (read-bytes-from-socket (:socket @socket-connection) packet)
    (dg/datagram->message (.getData packet))))

(defn- read-messages [socket-connection datagram-bytes timeout]
  (->> (range timeout)
       (map (fn [& _] (read-message socket-connection datagram-bytes 1)))
       (filter #(not (nil? %)))))

(defn read-message-with-collision-detection
  "Tries to read messages each ms and return the message only if just one message was received"
  [{:keys [socket-connection config]} timeout]
  (let [messages (read-messages socket-connection (get-in config [:config :datagram-bytes]) timeout)]
    (when (first messages)
      (if (= 1 (count messages))
        (do
          (swap! socket-connection update-in [:received-messages] inc)
          (first messages))
        (log/info "Collision detected")))))

(defn- send-bytes-datagram-socket [^DatagramSocket socket ^DatagramPacket datagram]
  (.send socket datagram))

(defn- send-message [socket-connection message]
  (log/info "sending message:" message)
  (let [^bytes datagram-bytes (dg/message->datagram message)]
    (log/debug "sending datagram:" (into [] datagram-bytes))
    (->> (new DatagramPacket
              datagram-bytes
              (alength datagram-bytes)
              (InetAddress/getByName (:address @socket-connection))
              (:port @socket-connection))
         (send-bytes-datagram-socket (:socket @socket-connection)))
    (swap! socket-connection update-in [:send-messages] inc)))

(defn send-message-collision-safe? [{:keys [socket-connection]} message]
  (let [received-messages (read-messages socket-connection 34 (clk/ms-until-slot-middle 1000 25 (:slot message)))]
    (if (= 0 (count received-messages))
      (do (send-message socket-connection (assoc message :send-time clk/current-time))
          (clk/wait-until-slot-end 40)
          true)
      (do (clk/wait-until-slot-end 40)
          false))))

(defn disconnect-socket [{:keys [socket-connection]}]
  (try
    (log/info "leaving multicast group")
    (.leaveGroup (:socket @socket-connection)
                 (InetAddress/getByName (:address @socket-connection)))
    (log/info "closing socket")
    (.close (:socket @socket-connection))
    (catch Exception e
      (log/warn "Socket could not be disconnected, maybe none present"))))

(defn attach-socket [{:keys [socket-connection] :as self}]
  (disconnect-socket self)
  (log/info (format "joined socket at %s:%s %s" (:address @socket-connection) (:port @socket-connection) (:interface @socket-connection)))
  (let [multicast-socket (new MulticastSocket (:port @socket-connection))]
    (.setTimeToLive multicast-socket 1)
    (.setNetworkInterface multicast-socket (NetworkInterface/getByName (:interface @socket-connection)))
    (.joinGroup multicast-socket (InetAddress/getByName (:address @socket-connection)))
    (swap! socket-connection assoc :socket multicast-socket)))

(defrecord Connector [config app-status]
  c/Lifecycle
  (start [self]
    (log/info "-> starting Receiver Component")
    (System/setProperty "java.net.preferIPv4Stack" "true")
    (let [config-params (:config config)
          state-atom (socket-atom (:interface-name config-params) (:multicast-address config-params) (:socket-port config-params))
          new-self (assoc self :socket-connection state-atom)]
      (attach-socket new-self)
      new-self))

  (stop [self]
    (log/info "<- stopping Receiver Component")
    (disconnect-socket self)))

(defn new-connector []
  (map->Connector {}))
