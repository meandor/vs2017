(ns de.haw.vs.data-access.connector
  (:require [clojure.tools.logging :as log]
            [com.stuartsierra.component :as c]
            [de.otto.tesla.stateful.app-status :as appstat]
            [de.otto.status :as stat]
            [de.haw.vs.data-access.datagram :as dg])
  (:import (java.net InetAddress NetworkInterface MulticastSocket DatagramPacket DatagramSocket)))

(defn socket-atom [interface-name address port]
  (atom {:socket nil :received-messages 0 :send-messages 0 :address address :interface interface-name :port port}))

(defn read-bytes-from-socket [^MulticastSocket socket ^DatagramPacket packet]
  (.receive socket packet))

(defn read-message [{:keys [socket-connection config]} timeout]
  (let [buffer (byte-array (get-in [:config :datagram-bytes] config))
        packet (new DatagramPacket buffer (count buffer))]
    (read-bytes-from-socket (:socket @socket-connection) packet)
    (dg/datagram->message (.getData packet))))

(defn send-bytes-datagram-socket [^DatagramSocket socket ^DatagramPacket datagram]
  (.send socket datagram))

(defn send-datagram [{:keys [socket-connection]} ^bytes datagram-bytes]
  (log/info "sending datagram:" (into [] datagram-bytes))
  (->> (new DatagramPacket
            datagram-bytes
            (alength datagram-bytes)
            (InetAddress/getByName (:address @socket-connection))
            (:port @socket-connection))
       (send-bytes-datagram-socket (:socket @socket-connection)))
  (swap! socket-connection update-in [:send-messages] inc))

(defn disconnect-socket [{:keys [socket-connection]}]
  (try
    (when (= MulticastSocket (class (:socket @socket-connection)))
      (log/info "leaving multicast group")
      (.leaveGroup (:socket @socket-connection)
                   (InetAddress/getByName (:address @socket-connection))))
    (log/info "closing socket")
    (.close (:socket @socket-connection))
    (catch Exception e
      (.printStackTrace e))))

(defn attach-server-socket [{:keys [socket-connection] :as self}]
  (disconnect-socket self)
  (log/info "attaching server socket")
  (swap! socket-connection assoc :socket (new DatagramSocket (:port @socket-connection))))

(defn attach-client-socket [{:keys [socket-connection] :as self}]
  (disconnect-socket self)
  (log/info (format "joined socket at %s:%s %s" (:address @socket-connection) (:port @socket-connection) (:interface @socket-connection)))
  (let [multicast-socket (new MulticastSocket (:port @socket-connection))]
    (.setTimeToLive multicast-socket 1)
    (.setNetworkInterface multicast-socket (NetworkInterface/getByName (:interface @socket-connection)))
    (.joinGroup multicast-socket (InetAddress/getByName (:address @socket-connection)))
    (swap! socket-connection assoc :socket multicast-socket)))

(defn status [socket-atom]
  (if (nil? (:socket @socket-atom))
    (stat/status-detail :connector :error "No socket attached")
    (stat/status-detail :connector :ok (update @socket-atom :socket (fn [socket-class] (str (class socket-class)))))))

(defrecord Connector [config app-status]
  c/Lifecycle
  (start [self]
    (log/info "-> starting Receiver Component")
    (let [config-params (:config config)
          state-atom (socket-atom (:interface-name config-params) (:multicast-address config-params) (:socket-port config-params))
          new-self (assoc self :socket-connection state-atom)]
      (attach-client-socket new-self)
      (appstat/register-status-fun app-status #(status state-atom))
      new-self))

  (stop [self]
    (log/info "<- stopping Receiver Component")
    (disconnect-socket self)))

(defn new-connector []
  (map->Connector {}))
