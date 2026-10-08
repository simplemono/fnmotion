(ns fnmotion.captions-test
  (:require [clojure.test :refer [deftest is testing]]
            [fnmotion.captions :as captions]))

(def transcript
  {:words [{:start 0.0 :end 0.3 :text "Welcome"}
           {:start 0.3 :end 0.5 :text "to"}
           {:start 0.6 :end 1.0 :text "the"}
           {:start 1.0 :end 1.6 :text "show,"}
           {:start 2.0 :end 2.4 :text "I'm"}
           {:start 2.4 :end 2.9 :text "Max."}]})

(deftest fill-breaks-test
  (let [words (captions/fill-breaks 0.2 (:words transcript))]
    (testing "a short pause is filled"
      (is (= 0.6 (:end (second words)))))
    (testing "a long pause only up to the maximum"
      (is (= 1.8 (:end (nth words 3)))))
    (testing "the last word is untouched"
      (is (= 2.9 (:end (last words)))))))

(deftest blocks-test
  (let [blocks (captions/blocks 12 (:words transcript))]
    (testing "halved until every block fits, so the blocks come out even"
      (is (= ["Welcome to the" "show, I'm Max."] (map captions/text blocks)))
      (is (= [0.0 1.0] (map :start blocks)))
      (is (= [1.0 2.9] (map :end blocks)))))
  (testing "a single long word is a block of its own"
    (is (= 1 (count (captions/blocks 3 [{:start 0 :end 1 :text "Unbelievable"}])))))
  (is (empty? (captions/blocks 12 []))))

(deftest captions-at-test
  (let [blocks (captions/captions transcript {:max-chars 12})]
    (is (= "Welcome to the" (captions/text (captions/at blocks 0.55))))
    (is (= "show, I'm Max." (captions/text (captions/at blocks 2.0))))
    (testing "nothing after the end"
      (is (nil? (captions/at blocks 3.5))))
    (testing "the word that is spoken"
      (is (= "show," (:text (captions/word-at (:words transcript) 1.2))))
      (is (nil? (captions/word-at (:words transcript) 1.8))))))
