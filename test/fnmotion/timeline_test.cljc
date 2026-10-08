(ns fnmotion.timeline-test
  (:require [clojure.test :refer [deftest is testing]]
            [fnmotion.timeline :as tl]))

(def title
  (tl/scene 2 (fn [p] [:h1 {:style {:opacity p}} "Hello"])))

(def card
  (tl/still 3 [:p "and then"]))

(deftest at-test
  (is (= [:h1 {:style {:opacity 0.5}} "Hello"] (tl/at title 1)))
  (testing "clamped to the scene"
    (is (= [:h1 {:style {:opacity 1}} "Hello"] (tl/at title 5)))
    (is (= [:h1 {:style {:opacity 0}} "Hello"] (tl/at title -1)))))

(deftest sequence-test
  (let [both (tl/sequence title card)]
    (is (= 5 (:duration both)))
    (is (= [:h1 {:style {:opacity 0.5}} "Hello"] (tl/at both 1)))
    (testing "at the end of a scene the next one shows"
      (is (= [:p "and then"] (tl/at both 2))))
    (is (= [:p "and then"] (tl/at both 4.9)))
    (testing "the last scene stays"
      (is (= [:p "and then"] (tl/at both 7))))))

(deftest parallel-test
  (let [both (tl/parallel title card)]
    (is (= 3 (:duration both)))
    (is (= [[:h1 {:style {:opacity 0.5}} "Hello"] [:p "and then"]] (vec (tl/at both 1))))
    (testing "the shorter scene stays at its end"
      (is (= [[:h1 {:style {:opacity 1}} "Hello"] [:p "and then"]] (vec (tl/at both 2.5)))))
    (testing "a fragment is a seq, which the renderer splices"
      (is (seq? (tl/at both 1))))))

(deftest repeat-test
  (let [twice (tl/repeat 2 title)]
    (is (= 4 (:duration twice)))
    (is (= [:h1 {:style {:opacity 0.5}} "Hello"] (tl/at twice 1)))
    (is (= [:h1 {:style {:opacity 0.5}} "Hello"] (tl/at twice 3)))
    (is (= [:h1 {:style {:opacity 1}} "Hello"] (tl/at twice 4)))))

(deftest offset-and-window-test
  (let [later (tl/offset 1 title)]
    (is (= 3 (:duration later)))
    (is (nil? (tl/at later 0.5)))
    (is (= [:h1 {:style {:opacity 0.5}} "Hello"] (tl/at later 2))))
  (is (= 0 (tl/window 0.1 10 2 4)))
  (is (= 0.5 (tl/window 0.3 10 2 4)))
  (is (= 1 (tl/window 0.9 10 2 4))))
