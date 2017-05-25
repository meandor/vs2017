(ns de.haw.vs.networking.datagram-test
  (:require [clojure.test :refer :all]
            [de.haw.vs.networking.datagram :as d]))

(def now 1495394082495)

(deftest timestamp-to-byte-array-test
  (testing "should transform a timestamp in ms to byte array of size 8"
    (is (= [(byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)
            (byte 0x00) (byte 0x00) (byte 0x5) (byte 0x39)]
           (into [] (d/timestamp->bytes 1337))))

    (is (= [(byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)
            (byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)]
           (into [] (d/timestamp->bytes 0))))

    (is (= [(byte 0x00) (byte 0x00) (byte 0x01) (byte 0x5C)
            (byte 0x2C) (byte 0x6E) (byte -34) (byte -65)]
           (into [] (d/timestamp->bytes now))))

    (is (= [(byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)
            (byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)]
           (into [] (d/timestamp->bytes 18446744073709551616))))))

(deftest payload-to-byte-array-test
  (testing "should transform a payload string to byte array of size 24"
    (is (= [(byte 0x66) (byte 0x6f) (byte 0x6f) (byte 0x62)
            (byte 0x61) (byte 0x72) (byte 0x00) (byte 0x00)
            (byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)
            (byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)
            (byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)
            (byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)]
           (into [] (d/payload->bytes "foobar"))))

    (is (= [(byte 0x66) (byte 0x6f) (byte 0x6f) (byte 0x62)
            (byte 0x61) (byte 0x72) (byte 0x66) (byte 0x6f)
            (byte 0x6f) (byte 0x62) (byte 0x61) (byte 0x72)
            (byte 0x66) (byte 0x6f) (byte 0x6f) (byte 0x62)
            (byte 0x61) (byte 0x72) (byte 0x66) (byte 0x6f)
            (byte 0x6f) (byte 0x62) (byte 0x61) (byte 0x72)]
           (into [] (d/payload->bytes "foobarfoobarfoobarfoobarfoobar1234"))))))

(deftest transform-message-to-datagram-test
  (testing "should convert a message into a datagram of size 34"
    (is (= [(byte 0x0)
            (byte 0x74) (byte 0x65) (byte 0x61) (byte 0x6D)
            (byte 0x20) (byte 0x30) (byte 0x31) (byte 0x2D)
            (byte 0x30) (byte 0x31) (byte 0x70) (byte 0x61)
            (byte 0x79) (byte 0x6C) (byte 0x6F) (byte 0x61)
            (byte 0x64) (byte 0x00) (byte 0x00) (byte 0x00)
            (byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)
            (byte 0x03)
            (byte 0x00) (byte 0x00) (byte 0x01) (byte 0x5C)
            (byte 0x2C) (byte 0x6E) (byte -34) (byte -65)]
           (into [] (d/message->datagram {:station-class   "A"
                                          :station-name    "team 01-01"
                                          :payload-content "payload"
                                          :payload         "this is ignored"
                                          :slot            3
                                          :send-time       now}))))))

(deftest byte-array-empty?-test
  (testing "should test the given byte array"
    (is (= true
           (d/byte-array-empty? nil)))
    (is (= true
           (d/byte-array-empty? (byte-array 5))))
    (is (= false
           (d/byte-array-empty? (byte-array 5 [1 2 3 4 5]))))))

(deftest transform-datagram-to-message-test
  (testing "only convert non-empty datagrams"
    (is (= nil
           (d/datagram->message nil)))
    (is (= nil
           (d/datagram->message (byte-array 34)))))
  (testing "should convert a datagram of size 34 into a message"
    (is (= {:station-class   "B"
            :station-name    "team 01-01"
            :payload-content "payload!"
            :payload         "team 01-01payload!"
            :slot            4
            :send-time       now}
           (d/datagram->message (byte-array 34 [(byte 0x1)
                                                (byte 0x74) (byte 0x65) (byte 0x61) (byte 0x6D)
                                                (byte 0x20) (byte 0x30) (byte 0x31) (byte 0x2D)
                                                (byte 0x30) (byte 0x31) (byte 0x70) (byte 0x61)
                                                (byte 0x79) (byte 0x6C) (byte 0x6F) (byte 0x61)
                                                (byte 0x64) (byte 0x21) (byte 0x00) (byte 0x00)
                                                (byte 0x00) (byte 0x00) (byte 0x00) (byte 0x00)
                                                (byte 0x04)
                                                (byte 0x00) (byte 0x00) (byte 0x01) (byte 0x5C)
                                                (byte 0x2C) (byte 0x6E) (byte -34) (byte -65)]))))))