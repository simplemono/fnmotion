(ns fnmotion.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [fnmotion.core :as fm]))

(defn- close?
  [a b]
  (< (Math/abs (- a b)) 1e-9))

(deftest progress-test
  (is (= 0 (fm/progress 1 2 4)))
  (is (close? 0.5 (fm/progress 3 2 4)))
  (is (= 1 (fm/progress 5 2 4)))
  (testing "an empty window is a step"
    (is (= 0 (fm/progress 1 2 2)))
    (is (= 1 (fm/progress 2 2 2)))))

(deftest easings-test
  (doseq [easing [fm/linear fm/quad-in fm/cubic-in fm/quint-in fm/quad-out fm/cubic-out
                  fm/quad-in-out fm/cubic-in-out fm/spring]]
    (testing "every easing starts at 0 and ends at 1"
      (is (close? 0 (easing 0)))
      (is (close? 1 (easing 1)))))
  (testing "ease-out mirrors ease-in"
    (is (close? (- 1 (fm/quad-in 0.7)) (fm/quad-out 0.3))))
  (testing "in-out meets in the middle"
    (is (close? 0.5 (fm/cubic-in-out 0.5))))
  (testing "a spring overshoots"
    (is (< 1 (apply max (map #(fm/spring (/ % 20)) (range 21))))))
  (is (close? 0 (fm/oscillate 0)))
  (is (close? 1 (fm/oscillate 0.25)))
  (is (= 0 (fm/delay 0.2 0.5)))
  (is (close? 0.5 (fm/delay 0.75 0.5)))
  (is (close? 0.5 (fm/limit 0.25 0.5)))
  (is (= 1 (fm/limit 0.9 0.5))))

(deftest interpolate-test
  (is (close? 25 (fm/interpolate 1.25 [1 2] [0 100])))
  (testing "clamped to the output range"
    (is (close? 0 (fm/interpolate 0 [1 2] [0 100])))
    (is (close? 100 (fm/interpolate 9 [1 2] [0 100]))))
  (testing "the output may run backwards"
    (is (close? 75 (fm/interpolate 1.25 [1 2] [100 0]))))
  (testing "with an easing"
    (is (close? (fm/lerp 0 100 (fm/cubic-out 0.25))
                (fm/interpolate 1.25 [1 2] [0 100] {:easing fm/cubic-out})))))

(deftest css-test
  (is (= "12px" (fm/px 12)))
  (is (= "12px" (fm/px 12.0)))
  (is (= "12.5px" (fm/px 12.5)))
  (is (= "50%" (fm/pct 0.5)))
  (is (= "33.5%" (fm/pct 0.335))))
