(ns de.haw.vs.networking.connector-test
  (:require [clojure.test :refer :all]
            [de.otto.tesla.util.test-utils :refer :all]
            [de.haw.vs.core :as core]
            [clj-http.client :as http]
            [clojure.data.json :as json]
            [de.haw.vs.networking.connector :as con]
            [de.haw.vs.networking.datagram :as dg]
            [clojure.tools.logging :as log])
  (:import (java.net DatagramSocket DatagramPacket)))

(defn test-system [runtime-config]
  (-> (core/station-system runtime-config)
      (dissoc :station
              :payload-source
              :message-writer)))

(deftest connector-socket-tests
  (with-started [system (test-system {:interface-name    "lo"
                                      :multicast-address "239.255.255.255"
                                      :socket-port       15001})]
                (testing "should startup the connector and establish the socket multicast connection"
                  (let [status-page (http/get "http://localhost:8080/status")
                        status-map (json/read-str (:body status-page) :key-fn keyword)]
                    (is (= {:message {:address           "239.255.255.255"
                                      :port              15001
                                      :interface         "lo"
                                      :received-messages 0
                                      :send-messages     0
                                      :socket            "class java.net.MulticastSocket"}
                            :status  "OK"}
                           (get-in status-map [:application :statusDetails :connector])))))

                (testing "should switch the socket to datagram socket"
                  (con/attach-server-socket (:connector system))
                  (let [status-page (http/get "http://localhost:8080/status")
                        status-map (json/read-str (:body status-page) :key-fn keyword)]
                    (is (= {:message {:address           "239.255.255.255"
                                      :port              15001
                                      :interface         "lo"
                                      :received-messages 0
                                      :send-messages     0
                                      :socket            "class java.net.DatagramSocket"}
                            :status  "OK"}
                           (get-in status-map [:application :statusDetails :connector])))))

                (testing "should switch the socket back to multicast socket"
                  (con/attach-client-socket (:connector system))
                  (let [status-page (http/get "http://localhost:8080/status")
                        status-map (json/read-str (:body status-page) :key-fn keyword)]
                    (is (= {:message {:address           "239.255.255.255"
                                      :port              15001
                                      :interface         "lo"
                                      :received-messages 0
                                      :send-messages     0
                                      :socket            "class java.net.MulticastSocket"}
                            :status  "OK"}
                           (get-in status-map [:application :statusDetails :connector])))))))

(def test-message
  {:payload         "!!!!!!!!!!!!!!!!!!!!!!!!"
   :payload-content "!!!!!!!!!!!!!!"
   :send-time       2387225703656530209
   :slot            33
   :station-class   "B"
   :station-name    "!!!!!!!!!!"})

(def test-message-datagram-bytes
  (byte-array 34 (conj (repeat 33 0x21) (byte 0x42))))

(deftest send-message-test
  (testing "send a message through the socket"
    (with-redefs [con/send-bytes-datagram-socket (fn [socket ^DatagramPacket datagram]
                                                   (is (not= nil socket))
                                                   (is (= (into [] test-message-datagram-bytes) (into [] (.getData datagram)))))]
      (let [socket-atom (con/socket-atom "lo" "239.255.255.255" 15001)
            connector {:socket-connection socket-atom}]
        (con/attach-server-socket connector)
        (con/send-message connector test-message)

        (is (= {:address           "239.255.255.255"
                :interface         "lo"
                :port              15001
                :received-messages 0
                :send-messages     1}
               (dissoc @socket-atom :socket)))
        (con/disconnect-socket connector)))))

(deftest receive-messages-test
  (testing "receive no message through the socket"
    (let [socket-atom (con/socket-atom "lo" "239.255.255.255" 15001)
          connector {:socket-connection socket-atom
                     :config            {:config {:datagram-bytes 34}}}]
      (with-redefs [con/read-bytes-from-socket (fn [socket ^DatagramPacket datagram]
                                                 nil)]
        (con/attach-client-socket connector)

        (is (= nil
               (con/read-message-with-collision-detection connector 2000)))

        (is (= {:address           "239.255.255.255"
                :interface         "lo"
                :port              15001
                :received-messages 0
                :send-messages     0}
               (dissoc @socket-atom :socket)))
        (con/disconnect-socket connector))))

  (testing "receive one message through the socket"
    (let [socket-atom (con/socket-atom "lo" "239.255.255.255" 15001)
          once (atom true)
          connector {:socket-connection socket-atom
                     :config            {:config {:datagram-bytes 34}}}]
      (with-redefs [con/read-bytes-from-socket (fn [socket ^DatagramPacket datagram]
                                                 (is (not= nil socket))
                                                 (is (= (into [] (byte-array 34)) (into [] (.getData datagram))))
                                                 (when @once
                                                   (.setData datagram test-message-datagram-bytes)
                                                   (reset! once false)))]
        (con/attach-client-socket connector)

        (is (= test-message
               (con/read-message-with-collision-detection connector 2000)))

        (is (= {:address           "239.255.255.255"
                :interface         "lo"
                :port              15001
                :received-messages 1
                :send-messages     0}
               (dissoc @socket-atom :socket)))
        (con/disconnect-socket connector))))

  (testing "should detect collision because more than one message was send during timeout"
    (with-redefs [con/read-bytes-from-socket (fn [socket ^DatagramPacket datagram]
                                               (is (not= nil socket))
                                               (is (= (into [] (byte-array 34)) (into [] (.getData datagram))))
                                               (.setData datagram test-message-datagram-bytes))]
      (let [socket-atom (con/socket-atom "lo" "239.255.255.255" 15001)
            connector {:socket-connection socket-atom
                       :config            {:config {:datagram-bytes 34}}}]
        (con/attach-client-socket connector)

        (is (= nil
               (con/read-message-with-collision-detection connector 2000)))

        (is (= {:address           "239.255.255.255"
                :interface         "lo"
                :port              15001
                :received-messages 0
                :send-messages     0}
               (dissoc @socket-atom :socket)))
        (con/disconnect-socket connector)))))
