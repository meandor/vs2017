(ns de.haw.vs.data-access.connector-test
  (:require [clojure.test :refer :all]
            [de.otto.tesla.util.test-utils :refer :all]
            [de.haw.vs.station.core :as core]
            [clj-http.client :as http]
            [clojure.data.json :as json]))

(deftest start-connector-test
  (testing "should startup the connector and establish the socket multicast connection"
    (with-started [system (core/station-system {})]
                  (let [status-page (http/get "http://localhost:8080/status")
                        status-map (json/read-str (:body status-page) :key-fn keyword)]
                    (is (= {:message {:address           "225.10.1.2"
                                      :port              15001
                                      :received-messages 0
                                      :send-messages     0}
                            :status  "OK"}
                           (get-in status-map [:application :statusDetails :connector])))))))
