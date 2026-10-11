(ns fnmotion.html-test
  (:require [clojure.test :refer [deftest is]]
            [fnmotion.html :as html]))

(deftest plain-test
  (is (= [:button {:class "x"} "Go"]
         (html/plain [:button {:class "x" :on {:click []} :replicant/key "k"} "Go"])))
  (is (= [:ul [:li {} "a"]]
         (html/plain [:ul [:li {:replicant/on-mount nil} "a"]]))))

(deftest html-test
  (is (= "<div style=\"opacity:0.5;\"><b>x</b>&lt;</div>"
         (html/html [:div {:style {:opacity 0.5} :on {:click []}} (list [:b "x"] nil "<")])))
  (is (= "<h1 class=\"a b\" data-t=\"1.5\">Hi</h1>"
         (html/html [:h1 {:class ["a" "b"] :data-t 1.5} "Hi"]))))
