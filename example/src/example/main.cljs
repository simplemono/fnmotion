(ns example.main
  "A ten-second short video: a title that springs in, captions from word
  timings, a progress bar, a closing card. The whole thing is `video`, a
  value; the player scrubs it."
  (:require [fnmotion.captions :as captions]
            [fnmotion.core :as fm]
            [fnmotion.player :as player]
            [fnmotion.timeline :as tl]))

(def transcript
  {:words [{:start 0.4 :end 0.8 :text "Every"}
           {:start 0.8 :end 1.1 :text "frame"}
           {:start 1.1 :end 1.3 :text "is"}
           {:start 1.3 :end 1.5 :text "a"}
           {:start 1.5 :end 2.1 :text "function"}
           {:start 2.1 :end 2.3 :text "of"}
           {:start 2.3 :end 2.8 :text "time."}
           {:start 3.3 :end 3.6 :text "Drag"}
           {:start 3.6 :end 3.8 :text "the"}
           {:start 3.8 :end 4.3 :text "slider"}
           {:start 4.3 :end 4.5 :text "and"}
           {:start 4.5 :end 4.9 :text "see"}
           {:start 4.9 :end 5.1 :text "it"}
           {:start 5.1 :end 5.6 :text "change."}]})

(def blocks
  (captions/captions transcript {:max-chars 20}))

(def intro
  "Six seconds: title springs in, captions below, a bar at the bottom."
  (tl/scene 6
            (fn [p]
              (let [t (* p 6)
                    enter (fm/spring (fm/progress t 0 1.2))
                    block (captions/at blocks t)
                    word (captions/word-at (:words transcript) t)]
                [:div.stage {:style {:background (str "hsl(" (fm/interpolate t [0 6] [210 290]) " 60% 20%)")}}
                 [:h1 {:style {:transform (str "translateY(" (fm/px (fm/lerp 80 0 enter)) ") scale(" (fm/lerp 0.6 1 enter) ")")
                               :opacity (fm/progress t 0 0.4)}}
                  "fnmotion"]
                 [:p.caption
                  (when block
                    (for [w (:words block)]
                      [:span {:class (when (= w word) "on")} (:text w) " "]))]
                 [:div.bar {:style {:width (fm/pct p)}}]]))))

(def outro
  (tl/scene 4
            (fn [p]
              [:div.stage {:style {:background "#111"}}
               [:div.card {:style {:opacity (fm/cubic-out (fm/limit p 0.3))
                                   :transform (str "translateY(" (fm/px (fm/interpolate p [0 0.3] [40 0] {:easing fm/cubic-out})) ")")}}
                [:h2 "A frame is a function of time."]
                [:p "Scenes are values, the player is a loop, the scrubber is the point."]]])))

(def video
  (tl/sequence intro outro))

(defn init!
  []
  (player/mount! {:element "#app"
                  :scene video}))
