(ns de.haw.vs.data-access.connector-test
  (:require [clojure.test :refer :all]
            [de.otto.tesla.util.test-utils :refer :all]
            [de.haw.vs.core :as core]
            [clj-http.client :as http]
            [clojure.data.json :as json]
            [de.haw.vs.data-access.connector :as con])
  (:import (java.net DatagramSocket DatagramPacket)))

(defn test-system [runtime-config]
  (-> (core/station-system runtime-config)
      (dissoc :station)))

(deftest connector-socket-tests
  (with-started [system (test-system {:interface-name    "eth0"
                                      :multicast-address "239.255.255.255"
                                      :socket-port       15001})]
                (testing "should startup the connector and establish the socket multicast connection"
                  (let [status-page (http/get "http://localhost:8080/status")
                        status-map (json/read-str (:body status-page) :key-fn keyword)]
                    (is (= {:message {:address           "239.255.255.255"
                                      :port              15001
                                      :interface         "eth0"
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
                                      :interface         "eth0"
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
                                      :interface         "eth0"
                                      :received-messages 0
                                      :send-messages     0
                                      :socket            "class java.net.MulticastSocket"}
                            :status  "OK"}
                           (get-in status-map [:application :statusDetails :connector])))))))

(deftest send-message-test
  (testing "send a message through the socket"
    (with-redefs [con/send-bytes-datagram-socket (fn [socket ^DatagramPacket datagram]
                                                   (is (not= nil socket))
                                                   (is (= [1 2 3] (into [] (.getData datagram)))))]
      (let [socket-atom (con/socket-atom "eth0" "239.255.255.255" 15001)
            connector {:socket-connection socket-atom}]
        (con/attach-server-socket connector)
        (con/send-datagram connector (byte-array 3 [1 2 3]))
        (is (= {:address           "239.255.255.255"
                :interface         "eth0"
                :port              15001
                :received-messages 0
                :send-messages     1}
               (dissoc @socket-atom :socket)))
        (con/disconnect-socket connector)))))

(deftest receive-message-test
  (testing "receive a message through the socket"
    (with-redefs [con/read-bytes-from-socket (fn [socket ^DatagramPacket datagram]
                                               (is (not= nil socket))
                                               (is (= 2000 (.getSoTimeout socket)))
                                               (is (= (into [] (byte-array 34)) (into [] (.getData datagram))))
                                               (.setData datagram (byte-array 34 [1 0x21 0x23])))]
      (let [socket-atom (con/socket-atom "eth0" "239.255.255.255" 15001)
            connector {:socket-connection socket-atom
                       :config            {:config {:datagram-bytes 34}}}]
        (con/attach-client-socket connector)

        (is (= {:payload         "!#"
                :payload-content ""
                :send-time       0
                :slot            0
                :station-class   "B"
                :station-name    "!#"}
               (con/read-message connector 2000)))

        (is (= {:address           "239.255.255.255"
                :interface         "eth0"
                :port              15001
                :received-messages 1
                :send-messages     0}
               (dissoc @socket-atom :socket)))
        (con/disconnect-socket connector)))))
