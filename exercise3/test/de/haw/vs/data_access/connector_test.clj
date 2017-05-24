(ns de.haw.vs.data-access.connector-test
  (:require [clojure.test :refer :all]
            [de.otto.tesla.util.test-utils :refer :all]
            [de.haw.vs.core :as core]
            [clj-http.client :as http]
            [clojure.data.json :as json]
            [de.haw.vs.data-access.connector :as con]))

(deftest start-connector-test
  (with-started [system (core/station-system {:interface-name    "eth0"
                                              :multicast-address "239.255.255.255"
                                              :socket-port       15001})]
                (testing "should startup the connector and establish the socket multicast connection"
                  (let [status-page (http/get "http://localhost:8080/status")
                        status-map (json/read-str (:body status-page) :key-fn keyword)]
                    (is (= {:message {:address           "239.255.255.255"
                                      :port              15001
                                      :network-interface "eth0"
                                      :received-messages 0
                                      :send-messages     0}
                            :status  "OK"}
                           (get-in status-map [:application :statusDetails :connector])))))

                (testing "send a message through the socket"
                  (is (not= nil
                            (:socket @(get-in system [:connector :socket-connection]))))
                  (con/send-datagram (byte-array 3 [1 2 3])
                                     (get-in system [:connector :socket-connection])))))
