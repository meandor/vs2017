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
    (with-redefs [clk/current-time 123]
      (is (= 12
             (clk/current-frame 10))))

    (with-redefs [clk/current-time 1537]
      (is (= 153
             (clk/current-frame 10))))))

(deftest current-slot-test
  (testing "Should return 1 as the current slot number"
    (with-redefs [clk/current-time 1]
      (is (= 1
             (clk/current-slot 120 3))))

    (with-redefs [clk/current-time 22]
      (is (= 1
             (clk/current-slot 120 3))))

    (with-redefs [clk/current-time 40]
      (is (= 1
             (clk/current-slot 120 3))))

    (with-redefs [clk/current-time 123]
      (is (= 1
             (clk/current-slot 120 3)))))

  (testing "Should return two as the current slot number"
    (with-redefs [clk/current-time 41]
      (is (= 2
             (clk/current-slot 120 3))))

    (with-redefs [clk/current-time 57]
      (is (= 2
             (clk/current-slot 120 3))))

    (with-redefs [clk/current-time 80]
      (is (= 2
             (clk/current-slot 120 3))))

    (with-redefs [clk/current-time 178]
      (is (= 2
             (clk/current-slot 120 3)))))

  (testing "Should return three as the current slot number"
    (with-redefs [clk/current-time 81]
      (is (= 3
             (clk/current-slot 120 3))))

    (with-redefs [clk/current-time 114]
      (is (= 3
             (clk/current-slot 120 3))))

    (with-redefs [clk/current-time 201]
      (is (= 3
             (clk/current-slot 120 3))))))

(deftest remaining-slots-test
  (testing "Should calculate the remaining  slots of frame size 453 and 3 slots"
    (with-redefs [clk/current-time 1]
      (is (= 2
             (clk/remaining-slots 120 3))))
    (with-redefs [clk/current-time 41]
      (is (= 1
             (clk/remaining-slots 120 3))))
    (with-redefs [clk/current-time 81]
      (is (= 0
             (clk/remaining-slots 120 3))))))
