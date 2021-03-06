(ns de.haw.vs.networking.connector-test
  (:require [clojure.test :refer :all]
            [de.otto.tesla.util.test-utils :refer :all]
            [de.haw.vs.core :as core]
            [clojure.data.json :as json]
            [de.haw.vs.networking.connector :as con]
            [de.haw.vs.networking.datagram :as dg]
            [clojure.tools.logging :as log]
            [de.haw.vs.clock :as clk])
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
                  (is (= {:address           "239.255.255.255"
                          :port              15001
                          :interface         "lo"
                          :received-messages 0
                          :send-messages     0}
                         (dissoc @(get-in system [:connector :socket-connection]) :socket))))))

(def test-message
  {:payload         "!!!!!!!!!!!!!!!!!!!!!!!!"
   :payload-content "!!!!!!!!!!!!!!"
   :send-time       2387225703656530209
   :received-time   1337
   :slot            6
   :station-class   "B"
   :station-name    "!!!!!!!!!!"})

(def test-message-datagram-bytes
  (dg/message->datagram test-message))

(deftest send-message-test
  (let [socket-atom (con/socket-atom "lo" "239.255.255.255" 15001)
        connector {:socket-connection socket-atom}]
    (con/attach-socket connector)
    (testing "send a message through the socket"
      (with-redefs [clk/current-time (constantly 2387225703656530209)
                    con/read-messages (fn [_ datagram-bytes timeout]
                                        (is (= 34 datagram-bytes))
                                        (is (= 20 timeout))
                                        (Thread/sleep timeout)
                                        [])
                    con/send-bytes-datagram-socket (fn [socket ^DatagramPacket datagram]
                                                     (is (= (:socket @socket-atom) socket))
                                                     (is (= (into [] test-message-datagram-bytes) (into [] (.getData datagram)))))]
        (let [now (System/currentTimeMillis)]
          (is (= true
                 (con/send-message-collision-safe? connector test-message)))

          (is (= {:address           "239.255.255.255"
                  :interface         "lo"
                  :port              15001
                  :received-messages 0
                  :send-messages     1}
                 (dissoc @socket-atom :socket))))))

    (testing "do not send a message because collision was detected"
      (let [send-count (atom 0)]
        (with-redefs [clk/current-time (constantly 2387225703656530209)
                      con/read-messages (fn [_ datagram-bytes timeout]
                                          (is (= 34 datagram-bytes))
                                          (is (= 20 timeout))
                                          (Thread/sleep timeout)
                                          [1 2 3 4])
                      con/send-bytes-datagram-socket (fn [& _] (swap! send-count inc))]
          (let [now (System/currentTimeMillis)]
            (is (= false
                   (con/send-message-collision-safe? connector test-message)))
            (is (= 0
                   @send-count))

            (is (= {:address           "239.255.255.255"
                    :interface         "lo"
                    :port              15001
                    :received-messages 0
                    :send-messages     1}
                   (dissoc @socket-atom :socket)))))))
    (con/disconnect-socket connector)))

(deftest receive-message-with-collision-detection-timing-test
  (let [connector {:socket-connection (con/socket-atom "lo" "239.255.255.255" 15001)
                   :config            {:config {:datagram-bytes 34}}}]
    (con/attach-socket connector)

    (testing "Should only take 40 ms to read from the socket with no message"
      (let [before (System/currentTimeMillis)
            result (con/read-message-with-collision-detection connector 40)]
        (is (= nil
               result))))

    (testing "Should only take 40 ms to read from the socket with messages"
      (with-redefs [con/read-bytes-from-socket (fn [socket ^DatagramPacket datagram]
                                                 (.setData datagram test-message-datagram-bytes))]
        (let [before (System/currentTimeMillis)]
          (let [before (System/currentTimeMillis)
                result (con/read-message-with-collision-detection connector 40)]
            (is (= nil
                   result))))))))

(deftest receive-messages-test
  (testing "receive no message through the socket"
    (let [socket-atom (con/socket-atom "lo" "239.255.255.255" 15001)
          connector {:socket-connection socket-atom
                     :config            {:config {:datagram-bytes 34}}}]
      (with-redefs [con/read-bytes-from-socket (fn [socket ^DatagramPacket datagram]
                                                 nil)]
        (con/attach-socket connector)

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
      (with-redefs [clk/current-time (constantly 1337)
                    con/read-bytes-from-socket (fn [socket ^DatagramPacket datagram]
                                                 (is (not= nil socket))
                                                 (is (= (into [] (byte-array 34)) (into [] (.getData datagram))))
                                                 (when @once
                                                   (.setData datagram test-message-datagram-bytes)
                                                   (reset! once false)))]
        (con/attach-socket connector)

        (is (= test-message
               (con/read-message-with-collision-detection connector 2000)))
        (con/disconnect-socket connector))))

  (testing "should detect collision because more than one message was send during timeout"
    (with-redefs [con/read-bytes-from-socket (fn [socket ^DatagramPacket datagram]
                                               (is (not= nil socket))
                                               (is (= (into [] (byte-array 34)) (into [] (.getData datagram))))
                                               (.setData datagram test-message-datagram-bytes))]
      (let [socket-atom (con/socket-atom "lo" "239.255.255.255" 15001)
            connector {:socket-connection socket-atom
                       :config            {:config {:datagram-bytes 34}}}]
        (con/attach-socket connector)

        (is (= nil
               (con/read-message-with-collision-detection connector 2000)))

        (is (= {:address           "239.255.255.255"
                :interface         "lo"
                :port              15001
                :received-messages 0
                :send-messages     0}
               (dissoc @socket-atom :socket)))
        (con/disconnect-socket connector)))))
