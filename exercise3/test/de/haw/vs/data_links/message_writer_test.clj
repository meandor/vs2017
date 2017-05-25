(ns de.haw.vs.data-links.message-writer-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.core.async :as async]
            [de.haw.vs.data-links.message-writer :as mw]
            [de.otto.tesla.util.test-utils :refer :all]
            [clojure.string :as s]))

(defn test-message [slot]
  {:station-class   "A"
   :station-name    (str "station" slot)
   :payload-content "foobar"
   :payload         (str "station" slot "foobar")
   :slot            slot
   :send-time       (System/currentTimeMillis)})

(deftest write-message-to-file-test
  (testing "Should write messages from the channel into the file"
    (io/delete-file "./dev-resources/output.log" true)
    (let [input-channel (async/chan 10)
          expected-1 (test-message 1)
          expected-2 (test-message 2)
          expected-3 (test-message 3)]
      (async/go (async/>! input-channel expected-1))
      (async/go (async/>! input-channel expected-2))
      (async/go (async/>! input-channel expected-3))
      (mw/append-messages-to-file "./dev-resources/output.log" input-channel)
      (Thread/sleep 3000)
      (is (= 3
             (count (s/split (slurp "./dev-resources/output.log") #"\n")))))))
