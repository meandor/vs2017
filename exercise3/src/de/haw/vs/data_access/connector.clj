(ns de.haw.vs.data-access.connector
  (:require [com.stuartsierra.component :as c]
            [clojure.tools.logging :as log]
            [de.otto.tesla.stateful.app-status :as appstat]
            [de.otto.status :as stat])
  (:import (java.net MulticastSocket InetAddress DatagramPacket NetworkInterface)))

(defn send-datagram [^bytes datagram-bytes socket-connection-atom]
  (log/info "sending datagram:" (into [] datagram-bytes))
  (->> (new DatagramPacket
            datagram-bytes
            (alength datagram-bytes)
            (InetAddress/getByName (:address @socket-connection-atom))
            (:port @socket-connection-atom))
       (.send (:socket @socket-connection-atom))))

(defn leave [self]
  (log/info "leaving multicast group")
  (.leaveGroup (:socket @(:socket-connection self))
               (InetAddress/getByName (:address @(:socket-connection self)))))

(defn join [interface-name multicast-address socket-port socket-atom]
  (log/info (format "joined socket at %s:%s" multicast-address socket-port))
  (let [multicast-socket (new MulticastSocket socket-port)]
    (.setTimeToLive multicast-socket 1)
    (.setNetworkInterface multicast-socket (NetworkInterface/getByName interface-name))
    (.joinGroup multicast-socket (InetAddress/getByName multicast-address))
    (swap! socket-atom assoc
           :socket multicast-socket
           :address multicast-address
           :network-interface interface-name
           :port socket-port)))

(defn status [socket-atom]
  (if (nil? (:socket @socket-atom))
    (stat/status-detail :connector :error "No socket attached")
    (stat/status-detail :connector :ok (dissoc @socket-atom :socket))))

(defrecord Connector [config app-status]
  c/Lifecycle
  (start [self]
    (log/info "-> starting Connector Component")
    (let [socket (atom {:socket nil :send-messages 0 :received-messages 0})
          config-params (:config config)
          new-self (assoc self :socket-connection socket)]
      (join (:interface-name config-params) (:multicast-address config-params) (:socket-port config-params) socket)
      (appstat/register-status-fun app-status #(status socket))
      new-self))

  (stop [self]
    (log/info "<- stopping Connector Component")
    (leave self)
    self))

(defn new-connector []
  (map->Connector {}))
