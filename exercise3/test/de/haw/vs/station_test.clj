(ns de.haw.vs.station-test
  (:require [clojure.test :refer :all]
            [de.haw.vs.station :as stat]
            [de.haw.vs.networking.connector :as con]
            [clojure.core.async :as async]
            [de.otto.tesla.util.test-utils :refer :all]
            [de.haw.vs.clock :as clk]))

(deftest range-starting-with-one-test
  (is (= [1 2 3] (stat/range-starting-with-one 3)))

  (is (= [1 2 3 4] (stat/range-starting-with-one 4)))

  (is (= [1 2 3 4 5 6] (stat/range-starting-with-one 6))))

(deftest find-free-slots-test
  (testing "Should find one free slot"
    (is (= [1]
           (stat/find-free-slots [1 2 3] [{:station-class   "A"
                                           :station-name    "foo"
                                           :payload-content "foobar"
                                           :payload         "foo foobar"
                                           :slot            2
                                           :send-time       (System/currentTimeMillis)}
                                          {:station-class   "B"
                                           :station-name    "foo3"
                                           :payload-content "foobar3"
                                           :payload         "foo3 foobar3"
                                           :slot            3
                                           :send-time       (System/currentTimeMillis)}]))))
  (testing "Should find two free slot"
    (is (= [3 4 6]
           (stat/find-free-slots [2 3 4 5 6] [{:station-class   "A"
                                               :station-name    "foo"
                                               :payload-content "foobar"
                                               :payload         "foo foobar"
                                               :slot            2
                                               :send-time       (System/currentTimeMillis)}
                                              {:station-class   "B"
                                               :station-name    "foo3"
                                               :payload-content "foobar3"
                                               :payload         "foo3 foobar3"
                                               :slot            5
                                               :send-time       (System/currentTimeMillis)}])))))

(defn test-message [slot]
  {:station-class   "A"
   :station-name    (str "station" slot)
   :payload-content "foobar"
   :payload         (str "station" slot "foobar")
   :slot            slot})

(deftest read-messages-test
  (let [message-slot (atom 0)]
    (testing "Should read multiple messages from connector"
      (with-redefs [con/read-message-with-collision-detection (fn [connector timeout]
                                                                (swap! message-slot inc)
                                                                (is (= nil connector))
                                                                (is (= 40 timeout))
                                                                (test-message @message-slot))]
        (is (= [{:station-class   "A"
                 :station-name    "station1"
                 :payload-content "foobar"
                 :payload         "station1foobar"
                 :slot            1}
                {:station-class   "A"
                 :station-name    "station2"
                 :payload-content "foobar"
                 :payload         "station2foobar"
                 :slot            2}]
               (stat/read-messages nil 80 2)))))

    (testing "Should read also empty messages from connector"
      (reset! message-slot 0)
      (with-redefs [con/read-message-with-collision-detection (fn [connector timeout]
                                                                (swap! message-slot inc)
                                                                (is (= nil connector))
                                                                (is (= 40 timeout))
                                                                (when (= @message-slot 2)
                                                                  (test-message @message-slot)))]
        (is (= [{:station-class   "A"
                 :station-name    "station2"
                 :payload-content "foobar"
                 :payload         "station2foobar"
                 :slot            2}]
               (stat/read-messages nil 80 2)))))))

(deftest send-message-test
  (let [send-counter (atom 0)
        input-chan (async/chan 10)
        state-atom (atom {:slot          1
                          :station-class "A"
                          :utc-offset    2})]
    (with-redefs [clk/current-time 1
                  con/send-message-collision-safe? (fn [connector message]
                                                     (is (= nil connector))
                                                     (is (= (test-message 1) message))
                                                     (swap! send-counter inc)
                                                     true)]

      (testing "Should not send a message because channel is empty"
        (is (= false
               (stat/message-was-send? nil input-chan nil)))
        (is (= 0
               @send-counter)))

      (testing "Should send one message from the channel"
        (async/>!! input-chan (dissoc (test-message 1) :station-class :send-time :slot))
        (is (= true
               (stat/message-was-send? state-atom input-chan nil)))
        (is (= 1
               @send-counter))))))
