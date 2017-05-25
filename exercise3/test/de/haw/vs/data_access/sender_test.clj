(ns de.haw.vs.data-access.sender-test
  (:require [clojure.test :refer :all]
            [de.otto.tesla.util.test-utils :refer :all]
            [de.haw.vs.core :as core]
            [clj-http.client :as http]
            [clojure.data.json :as json]
            [de.haw.vs.data-access.sender :as s]))

(def test-system
  (-> (core/station-system {:socket-port 15001})
      (dissoc
        :receiver
        :station)))

(deftest start-sender-test
  (with-started [system test-system]
                (testing "should startup the sender and establish the socket connection"
                  (let [status-page (http/get "http://localhost:8080/status")
                        status-map (json/read-str (:body status-page) :key-fn keyword)]
                    (is (= {:message {:port          15001
                                      :send-messages 0}
                            :status  "OK"}
                           (get-in status-map [:application :statusDetails :sender])))))

                (testing "send a message through the socket"
                  (is (not= nil
                            (:socket @(get-in system [:sender :socket-connection]))))
                  (s/send-datagram (byte-array 3 [1 2 3])
                                   (get-in system [:sender :socket-connection])))))
