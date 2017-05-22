(ns de.haw.vs.data-access.connector
  (:require [com.stuartsierra.component :as c]
            [clojure.tools.logging :as log]
            [de.otto.tesla.stateful.app-status :as appstat]
            [de.otto.status :as stat])
  (:import (java.net MulticastSocket InetAddress DatagramPacket)))

(defn send-datagram [^bytes datagram-bytes socket-connection-atom]
  (log/info "sending datagram:" (into [] datagram-bytes))
  (->> (DatagramPacket.
         datagram-bytes
         (alength datagram-bytes)
         (InetAddress/getByName (:address @socket-connection-atom))
         (:port @socket-connection-atom))
       (.send (:socket @socket-connection-atom))))

(defn leave [self]
  (log/info "leaving multicast group")
  (.leaveGroup (:socket @(:socket-connection self))
               (InetAddress/getByName (:address @(:socket-connection self)))))

(defn join [config socket-atom]
  (let [multicast-socket (new MulticastSocket (:socket-port config 15001))]
    (.setTimeToLive multicast-socket (:socket-ttl config 1))
    (.joinGroup multicast-socket (InetAddress/getByName (:socket-address config "225.10.1.2")))
    (log/info "joined socket at " (:socket-address config "225.10.1.2") ":" (:socket-port config 15001))
    (swap! socket-atom assoc
           :socket multicast-socket
           :address (:socket-address config "225.10.1.2")
           :port (:socket-port config 15001))))

(defn status [socket-atom]
  (if (nil? (:socket @socket-atom))
    (stat/status-detail :connector :error "No socket attached")
    (stat/status-detail :connector :ok (dissoc @socket-atom :socket))))

(defrecord Connector [config app-status]
  c/Lifecycle
  (start [self]
    (log/info "-> starting Connector Component")
    (let [socket (atom {:socket nil :send-messages 0 :received-messages 0})
          new-self (assoc self :socket-connection socket)]
      (join (:config config) socket)
      (appstat/register-status-fun app-status #(status socket))
      new-self))

  (stop [self]
    (log/info "<- stopping Connector Component")
    (leave self)
    self))

(defn new-connector []
  (map->Connector {}))
