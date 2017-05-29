(ns de.haw.vs.station-test
  (:require [clojure.test :refer :all]
            [de.haw.vs.station :as stat]
            [de.haw.vs.networking.connector :as con]
            [clojure.core.async :as async]
            [de.otto.tesla.util.test-utils :refer :all]
            [de.haw.vs.clock :as clk]))

(deftest find-free-slots-test
  (testing "Should find one free slot"
    (is (= [1]
           (stat/find-free-slots 3 [{:station-class   "A"
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
    (is (= [2 4]
           (stat/find-free-slots 4 [{:station-class   "A"
                                     :station-name    "foo"
                                     :payload-content "foobar"
                                     :payload         "foo foobar"
                                     :slot            1
                                     :send-time       (System/currentTimeMillis)}
                                    {:station-class   "B"
                                     :station-name    "foo3"
                                     :payload-content "foobar3"
                                     :payload         "foo3 foobar3"
                                     :slot            3
                                     :send-time       (System/currentTimeMillis)}])))))

(defn test-message [slot]
  {:station-class   "A"
   :station-name    (str "station" slot)
   :payload-content "foobar"
   :payload         (str "station" slot "foobar")
   :slot            slot
   :send-time       slot})

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
                 :slot            1
                 :send-time       1}
                {:station-class   "A"
                 :station-name    "station2"
                 :payload-content "foobar"
                 :payload         "station2foobar"
                 :slot            2
                 :send-time       2}]
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
                 :slot            2
                 :send-time       2}]
               (stat/read-messages nil 80 2)))))))

(deftest read-phase!-test
  (let [message-slot (atom 0)
        output-chan (async/chan 10)]

    (testing "Should select 1 as only free slot in the state atom"
      (with-redefs [stat/put-message-on-channel (fn [_ messages] messages)
                    con/read-message-with-collision-detection (fn [connector timeout]
                                                                (swap! message-slot inc)
                                                                (is (= nil connector))
                                                                (is (= 40 timeout))
                                                                (when (not= 1 @message-slot)
                                                                  (test-message @message-slot)))]

        (is (= 1
               (stat/read-phase 120 3 nil nil) 1))))

    (testing "Should keep slot empty if no free slot is found and send messages on the channel"
      (reset! message-slot 0)
      (with-redefs [con/read-message-with-collision-detection (fn [connector timeout]
                                                                (swap! message-slot inc)
                                                                (is (= nil connector))
                                                                (is (= 40 timeout))
                                                                (test-message @message-slot))]

        (is (= nil
               (stat/read-phase 120 3 output-chan nil)))
        (eventually (is (= 3 (.count (.buf output-chan)))))))

    (testing "Should assign random slot from free slots"
      (with-redefs [stat/put-message-on-channel (fn [_ messages] messages)
                    rand-nth (constantly 4)
                    con/read-message-with-collision-detection (fn [connector timeout]
                                                                (is (= nil connector))
                                                                (is (= 24 timeout))
                                                                nil)]

        (is (= 4
               (stat/read-phase 120 5 nil nil)))))

    (testing "Should not read messages if no slots are given"
      (reset! message-slot 0)
      (with-redefs [stat/put-message-on-channel (fn [_ messages] messages)
                    con/read-message-with-collision-detection (fn [_ _]
                                                                (swap! message-slot inc)
                                                                nil)]

        (is (= nil
               (stat/read-phase 120 0 nil nil)))
        (is (= 0
               @message-slot))))

    (testing "Should not read messages if no duration is given"
      (reset! message-slot 0)
      (with-redefs [stat/put-message-on-channel (fn [_ messages] messages)
                    con/read-message-with-collision-detection (fn [_ _]
                                                                (swap! message-slot inc)
                                                                nil)]

        (is (= nil
               (stat/read-phase 0 10 nil nil)))
        (is (= 0
               @message-slot))))))

(deftest send-phase!-test
  (let [send-counter (atom 0)
        input-chan (async/chan 10)
        state-atom (atom {:slot          1
                          :station-class "A"
                          :utc-offset    2})]
    (with-redefs [clk/current-time 1
                  con/send-message (fn [connector message]
                                     (is (= nil connector))
                                     (is (= (test-message 1) message))
                                     (swap! send-counter inc))]

      (testing "Should only send a message if the channel has a message"
        (stat/send-phase nil input-chan nil)
        (is (= 0
               @send-counter)))

      (testing "Should send one message from the channel"
        (async/>!! input-chan (dissoc (test-message 1) :station-class :send-time :slot))
        (stat/send-phase state-atom input-chan nil)
        (is (= 1
               @send-counter))))))
