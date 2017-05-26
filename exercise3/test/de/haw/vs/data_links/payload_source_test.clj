(ns de.haw.vs.data-links.payload-source-test
  (:require [clojure.test :refer :all]
            [clojure.core.async :as async]
            [de.haw.vs.data-links.payload-source :as ps]
            [clojure.java.io :as io]
            [de.otto.tesla.util.test-utils :refer :all]))

(deftest char-array->payload-message-test
  (testing "Should turn content of char array to payload map"
    (is (= {:station-name    "team 01-01"
            :payload-content "  \u0001  �  d�\u0010\u007F  "
            :payload         "team 01-01  \u0001  �  d�\u0010\u007F  "}
           (ps/char-array->payload-message (char-array 24 "team 01-01  \u0001  �  d�\u0010\u007F  "))))))

(deftest send-messages-to-channel-test
  (testing "Should send byte chunks from an input stream to the channel"
    (let [output-chan (async/chan)]
      (ps/send-messages-to-channel 24 "./dev-resources/stdin.txt" output-chan)
      (let [results (reduce (fn [acc _] (conj acc (async/<!! output-chan))) [] (range 3))]
        (is (= 3 (count results)))))))
