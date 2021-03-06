(ns de.haw.vs.clock-test
  (:require [clojure.test :refer :all]
            [de.haw.vs.clock :as clk]))

(deftest offset-atom-test
  (testing "Should change the offset atom"
    (reset! clk/offset 0)
    (is (= 0
           @clk/offset))

    (reset! clk/offset 3)
    (is (= 3
           @clk/offset))))

(deftest current-frame-test
  (testing "Should return the current frame number"
    (with-redefs [clk/current-time (constantly 123)]
      (is (= 12
             (clk/current-frame 10))))

    (with-redefs [clk/current-time (constantly 1537)]
      (is (= 153
             (clk/current-frame 10))))))

(deftest current-slot-test
  (testing "Should return 1 as the current slot number"
    (with-redefs [clk/current-time (constantly 1)]
      (is (= 1
             (clk/current-slot 120 3))))

    (with-redefs [clk/current-time (constantly 22)]
      (is (= 1
             (clk/current-slot 120 3))))

    (with-redefs [clk/current-time (constantly 40)]
      (is (= 1
             (clk/current-slot 120 3))))

    (with-redefs [clk/current-time (constantly 123)]
      (is (= 1
             (clk/current-slot 120 3)))))

  (testing "Should return two as the current slot number"
    (with-redefs [clk/current-time (constantly 41)]
      (is (= 2
             (clk/current-slot 120 3))))

    (with-redefs [clk/current-time (constantly 57)]
      (is (= 2
             (clk/current-slot 120 3))))

    (with-redefs [clk/current-time (constantly 80)]
      (is (= 2
             (clk/current-slot 120 3))))

    (with-redefs [clk/current-time (constantly 178)]
      (is (= 2
             (clk/current-slot 120 3)))))

  (testing "Should return three as the current slot number"
    (with-redefs [clk/current-time (constantly 81)]
      (is (= 3
             (clk/current-slot 120 3))))

    (with-redefs [clk/current-time (constantly 114)]
      (is (= 3
             (clk/current-slot 120 3))))

    (with-redefs [clk/current-time (constantly 201)]
      (is (= 3
             (clk/current-slot 120 3))))))

(deftest remaining-slots-test
  (testing "Should calculate the remaining  slots of frame size 453 and 3 slots"
    (with-redefs [clk/current-time (constantly 1)]
      (is (= 2
             (clk/remaining-slots 120 3))))
    (with-redefs [clk/current-time (constantly 41)]
      (is (= 1
             (clk/remaining-slots 120 3))))
    (with-redefs [clk/current-time (constantly 81)]
      (is (= 0
             (clk/remaining-slots 120 3))))))

(deftest wait-until-slot-end-test
  (testing "Should wait for the slot with size 1000 to end"
    (with-redefs [clk/current-time (constantly 1495896445968)]
      (let [now (System/currentTimeMillis)]
        (clk/wait-until-slot-end 1000)
        (is (and (> (System/currentTimeMillis) (+ now 30))
                 (< (System/currentTimeMillis) (+ now 36)))))))

  (testing "Should wait for the slot with size 123 to end"
    (with-redefs [clk/current-time (constantly 1)]
      (let [now (System/currentTimeMillis)]
        (clk/wait-until-slot-end 123)
        (is (and (> (System/currentTimeMillis) (+ now 120))
                 (< (System/currentTimeMillis) (+ now 127))))))))

(deftest ms-until-slot-middle-test
  (with-redefs [clk/current-time (constantly 12)]
    (testing "Should return the time before the slot middle of the first slot in ms"
      (is (= 8
             (clk/ms-until-slot-middle 120 3 1))))

    (testing "Should return the time before the slot middle of the second slot in ms"
      (is (= 48
             (clk/ms-until-slot-middle 120 3 2))))

    (testing "Should return the time before the slot middle of the fourth slot in ms"
      (is (= 128
             (clk/ms-until-slot-middle 160 4 4))))))

(deftest process-message-test
  (testing "Should update the offset dependent on the message"
    (reset! clk/offset 0)

    (is (= 42
           (clk/process-message {:station-class "A"
                                 :send-time     1253
                                 :received-time 1337})))
    (is (= 42
           @clk/offset)))

  (testing "Should not update the offset because station class type B"
    (reset! clk/offset 0)

    (is (= nil
           (clk/process-message {:station-class "B"
                                 :send-time     1253
                                 :received-time 1337})))
    (is (= 0
           @clk/offset))))
