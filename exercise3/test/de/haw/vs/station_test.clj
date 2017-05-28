(ns de.haw.vs.station-test
  (:require [clojure.test :refer :all]
            [de.haw.vs.station :as stat]
            [de.haw.vs.networking.connector :as con]
            [clojure.core.async :as async]
            [de.otto.tesla.util.test-utils :refer :all]))

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
      (with-redefs [con/read-message (fn [connector timeout]
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
      (with-redefs [con/read-message (fn [connector timeout]
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
  (let [state-atom (atom {:slot nil})
        message-slot (atom 0)
        output-chan (async/chan 10)]

    (testing "Should select a free slot in the state atom"
      (with-redefs [stat/put-message-on-channel! (fn [_ messages] messages)
                    con/read-message (fn [connector timeout]
                                       (swap! message-slot inc)
                                       (is (= nil connector))
                                       (is (= 40 timeout))
                                       (when (not= 1 @message-slot)
                                         (test-message @message-slot)))]
        (stat/read-phase! state-atom 120 3 nil nil)
        (is (= {:slot 1}
               @state-atom))))

    (testing "Should keep slot empty if no free slot is found and send messages on the channel"
      (reset! state-atom {:slot nil})
      (reset! message-slot 0)
      (with-redefs [con/read-message (fn [connector timeout]
                                       (swap! message-slot inc)
                                       (is (= nil connector))
                                       (is (= 40 timeout))
                                       (test-message @message-slot))]
        (stat/read-phase! state-atom 120 3 output-chan nil)
        (is (= {:slot nil}
               @state-atom))
        (eventually (is (= 3 (.count (.buf output-chan)))))))

    (testing "Should assign slot one if no messages are received"
      (reset! state-atom {:slot nil})
      (with-redefs [stat/put-message-on-channel! (fn [_ messages] messages)
                    con/read-message (fn [connector timeout]
                                       (is (= nil connector))
                                       (is (= 40 timeout))
                                       nil)]
        (stat/read-phase! state-atom 120 3 nil nil)
        (is (= {:slot 1}
               @state-atom))))

    (testing "Should not change state if no slots are given"
      (reset! state-atom {:slot 3})
      (reset! message-slot 0)
      (with-redefs [stat/put-message-on-channel! (fn [_ messages] messages)
                    con/read-message (fn [_ _]
                                       (swap! message-slot inc)
                                       nil)]
        (stat/read-phase! state-atom 120 0 nil nil)
        (is (= {:slot 3}
               @state-atom))
        (is (= 0
               @message-slot))))

    (testing "Should not change state if no duration are given"
      (reset! state-atom {:slot 3})
      (reset! message-slot 0)
      (with-redefs [stat/put-message-on-channel! (fn [_ messages] messages)
                    con/read-message (fn [_ _]
                                       (swap! message-slot inc)
                                       nil)]
        (stat/read-phase! state-atom 0 10 nil nil)
        (is (= {:slot 3}
               @state-atom))
        (is (= 0
               @message-slot))))))

(deftest send-phase!-test
  (let [send-counter (atom 0)
        input-chan (async/chan 10)
        state-atom (atom {:slot          1
                          :station-class "A"
                          :utc-offset    2})]
    (with-redefs [stat/current-time (constantly 1)
                  con/send-message (fn [connector message]
                                     (is (= nil connector))
                                     (is (= (test-message 1) message))
                                     (swap! send-counter inc))]

      (testing "Should only send a message if the channel has a message"
        (stat/send-phase! nil input-chan nil)
        (is (= 0
               @send-counter)))

      (testing "Should send one message from the channel"
        (async/>!! input-chan (dissoc (test-message 1) :station-class :send-time :slot))
        (stat/send-phase! state-atom input-chan nil)
        (is (= 1
               @send-counter))))))

(deftest wait-for-next-phase!-test
  (with-redefs [stat/current-time (constantly 1495896445968)]
    (testing "Should wait for the next frame to start"
      (let [now (System/currentTimeMillis)]
        (stat/wait-for-next-phase! 1000 0)
        (is (and (> (System/currentTimeMillis) (+ now 30))
                 (< (System/currentTimeMillis) (+ now 34))))))

    (testing "Should wait for the next frame to start"
      (let [now (System/currentTimeMillis)]
        (stat/wait-for-next-phase! 1730 0)
        (is (and (> (System/currentTimeMillis) (+ now 740))
                 (< (System/currentTimeMillis) (+ now 744))))))))