(ns de.haw.vs.data-access.datagram-test
  (:require [clojure.test :refer :all]
            [clojure.spec.gen.alpha :as gen]
            [de.haw.vs.data-access.datagram :as d]
            [clojure.spec.alpha :as s]))

(def now 1495394082495)

(deftest number-to-byte-array-test
  (testing "should transform a timestamp in ms to byte array of size 8"
    (is (= [(byte 0x5) (byte 0x39) (byte 0x00) (byte 0x00)
            (byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)]
           (into [] (d/timestamp->bytes 1337))))))

(deftest payload-to-byte-array-test
  (testing "should transform a payload string to byte array of size 24"
    (is (= [(byte 0x66) (byte 0x6f) (byte 0x6f) (byte 0x62)
            (byte 0x61) (byte 0x72) (byte 0x00) (byte 0x00)
            (byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)
            (byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)
            (byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)
            (byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)]
           (into [] (d/payload->bytes "foobar"))))))

(deftest transform-message-to-datagram-test
  (testing "should convert a message into a datagram of size 34"
    (let [time (System/currentTimeMillis)]
      (is (= [(byte 0x0)
              (byte 0x70) (byte 0x61) (byte 0x79) (byte 0x6C)
              (byte 0x6F) (byte 0x61) (byte 0x64) (byte 0x00)
              (byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)
              (byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)
              (byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)
              (byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)
              (byte 0x03)
              (byte 0x01) (byte 0x5C) (byte 0x2C) (byte 0x6E)
              (byte -34) (byte -65) (byte 0x00) (byte 0x00)]
             (into [] (d/message->datagram {:station-class "A"
                                            :payload       "payload"
                                            :slot          3
                                            :send-time     now})))))))
