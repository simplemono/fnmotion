(ns fnmotion.rframes-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [fnmotion.core :as fm]
            [fnmotion.rframes :as player]))

(defn- frame
  [design t]
  [:h1 {:style {:opacity (fm/progress t 0 1)}} (:title design)])

(deftest frame-test
  (is (= [:h1 {:style {:opacity 0.5}} "Hi"] (player/frame frame {:title "Hi"} 0.5)))
  (is (= [:div nil] (player/frame (fn [_ _] [:div (fn [] 1)]) {} 0)))
  (let [[tag text] (player/frame (fn [_ _] (throw (ex-info "boom" {}))) {} 1.5)]
    (is (= :p.error tag))
    (is (re-find #"1[.,]5 s failed: boom" text))))

(deftest playback-test
  (let [db (atom {:design {:title "Hi"}})]
    (player/seek! db 4 250)
    (is (= 1.0 (:motion/t @db)))
    (is (false? (:motion/playing? @db)))
    (is (true? (player/toggle! db (fn [] 4))))
    (Thread/sleep 120)
    (is (< 1.05 (:motion/t @db) 4.0))
    (is (false? (player/toggle! db 4)))
    (testing "playing from the end starts over"
      (swap! db assoc :motion/t 4.0)
      (player/toggle! db 4)
      (Thread/sleep 40)
      (is (< (:motion/t @db) 1.0))
      (player/stop! db)
      (is (= 0.0 (:motion/t @db))))))

(deftest design-set-test
  (let [db (atom {:design {:size 20 :title "a"}})]
    (player/design-set! db [:size] "28")
    (player/design-set! db ["title"] "Hello")
    (player/design-set! db [:ratio] "0.5")
    (is (= {:size 28 :title "Hello" :ratio 0.5} (:design @db)))
    (is (= {:input [[:data/command {:command/kind :design/set
                                     :command/data {:path [:size] :value :event/target.value}}]]}
           (player/setter [:size])))))

(deftest watch-design-test
  (let [dir (str (java.nio.file.Files/createTempDirectory
                  "fnmotion-rframes"
                  (make-array java.nio.file.attribute.FileAttribute 0)))
        file (io/file dir "design.edn")
        db (player/watch-design! (atom {:design {:title "first"}}) file)]
    (swap! db assoc-in [:design :title] "second")
    (is (= "{:title \"second\"}" (slurp file)))
    (testing "a new atom reads it back"
      (is (= {:title "second"} (:design @(player/watch-design! (atom {:design {}}) file)))))))

(defn- fake-w
  [data]
  {:command {:command/data data}})

(deftest commands-and-view-test
  (let [db (atom {:design {:title "Hi"}})
        {:strs [toggle seek set]} (into {} (map (fn [{:keys [command/kind command/fn]}] [(name kind) fn]))
                                        (player/commands db {:duration 2}))]
    (is (= {:success? true} (:command/result (seek (fake-w {:value "500"})))))
    (is (= 1.0 (:motion/t @db)))
    (is (= {:success? true} (:command/result (set (fake-w {:path [:title] :value "Yo"})))))
    (is (= "Yo" (get-in @db [:design :title])))
    (is (false? (get-in (set (fake-w {:path "title" :value "x"})) [:command/result :success?])))
    (is (= {:success? true} (:command/result (toggle (fake-w {})))))
    (player/stop! db)
    (testing "the player renders the frame at t with the controls"
      (let [view (player/player (assoc @db :motion/t 1.0) {:frame frame :duration 2})
            strings (filter string? (tree-seq coll? seq view))
            range (first (filter #(and (vector? %) (= :input (first %))) (tree-seq coll? seq view)))]
        (is (= :div.motion (first view)))
        (is (some #{"Yo"} strings))
        (is (some #{"Play"} strings))
        (is (= 500 (:value (second range))))
        (is (some #{"0:01.0 / 0:02.0"} strings))))))
