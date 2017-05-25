(ns de.haw.vs.data-access.sender
  (:require [com.stuartsierra.component :as c]
            [de.otto.status :as stat]
            [clojure.tools.logging :as log]
            [de.otto.tesla.stateful.app-status :as appstat])
  (:import (java.net InetAddress DatagramPacket DatagramSocket)))

(defn send-datagram [^bytes datagram-bytes socket-connection-atom]
  (log/info "sending datagram:" (into [] datagram-bytes))
  (->> (new DatagramPacket
            datagram-bytes
            (alength datagram-bytes)
            (InetAddress/getByName (:address @socket-connection-atom))
            (:port @socket-connection-atom))
       (.send (:socket @socket-connection-atom))))

(defn status [socket-atom]
  (if (nil? (:socket @socket-atom))
    (stat/status-detail :sender :error "No socket attached")
    (stat/status-detail :sender :ok (dissoc @socket-atom :socket))))

(defn attach-socket [port socket-atom]
  (swap! socket-atom assoc
         :socket (new DatagramSocket port)
         :port port))

(defrecord Sender [config app-status]
  c/Lifecycle
  (start [self]
    (log/info "-> starting Receiver Component")
    (let [socket (atom {:socket nil :send-messages 0})
          config-params (:config config)
          new-self (assoc self :socket-connection socket)]
      (attach-socket (:socket-port config-params) socket)
      (appstat/register-status-fun app-status #(status socket))
      new-self))

  (stop [self]
    (log/info "<- stopping Receiver Component")
    (.close (:socket @(:socket-connection self)))
    self))

(defn new-sender []
  (map->Sender {}))
