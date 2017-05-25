(ns de.haw.vs.data-access.receiver
  (:require [com.stuartsierra.component :as c]
            [clojure.tools.logging :as log]
            [de.otto.tesla.stateful.app-status :as appstat]
            [de.otto.status :as stat]
            [de.haw.vs.data-access.datagram :as dg])
  (:import (java.net MulticastSocket InetAddress DatagramPacket NetworkInterface)))

(defn read-message [self]
  (let [buffer (byte-array (get-in [:config :config :datagram-bytes] self))
        packet (new DatagramPacket buffer (count buffer))]
    (.receive (:socket @(:socket-connection self)) packet)
    (dg/datagram->message (.getData packet))))

(defn leave [self]
  (log/info "leaving multicast group")
  (.leaveGroup (:socket @(:socket-connection self))
               (InetAddress/getByName (:address @(:socket-connection self))))
  (.close (:socket @(:socket-connection self))))

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
    (stat/status-detail :receiver :error "No socket attached")
    (stat/status-detail :receiver :ok (dissoc @socket-atom :socket))))

(defrecord Receiver [config app-status]
  c/Lifecycle
  (start [self]
    (log/info "-> starting Receiver Component")
    (let [socket (atom {:socket nil :received-messages 0})
          config-params (:config config)
          new-self (assoc self :socket-connection socket)]
      (join (:interface-name config-params) (:multicast-address config-params) (:socket-port config-params) socket)
      (appstat/register-status-fun app-status #(status socket))
      new-self))

  (stop [self]
    (log/info "<- stopping Receiver Component")
    (leave self)
    self))

(defn new-receiver []
  (map->Receiver {}))
