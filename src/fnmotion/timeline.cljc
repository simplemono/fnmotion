(ns fnmotion.timeline
  "Scenes and how they follow each other. A scene is a map

      {:duration seconds
       :render   (fn [p] hiccup)}   ; p is the progress 0..1 within the scene

  and the combinators make bigger scenes out of smaller ones. `at` is the
  frame of a scene at a time in seconds, the one function a player, a
  scrubber or a renderer needs."
  (:refer-clojure :exclude [sequence repeat]))

(defn scene
  "A scene of `duration` seconds rendered by `render`, a function of the
  progress 0..1."
  [duration render]
  {:duration duration
   :render render})

(defn still
  "A scene that shows the same hiccup for `duration` seconds."
  [duration hiccup]
  (scene duration (constantly hiccup)))

(defn frame
  "The hiccup of `scene` at progress `p` (0..1)."
  [{:keys [render]} p]
  (render (max 0 (min 1 p))))

(defn at
  "The hiccup of `scene` at time `t` in seconds."
  [{:keys [duration] :as scene} t]
  (frame scene (if (pos? duration)
                 (/ (double t) duration)
                 1)))

(defn- with-offsets
  "The scenes with the time each one starts at."
  [scenes]
  (second (reduce (fn [[start acc] scene]
                    [(+ start (:duration scene))
                     (conj acc (assoc scene :start start))])
                  [0 []]
                  scenes)))

(defn sequence
  "The scenes one after the other. At the exact end of a scene the next
  one shows; the last scene stays at its end."
  [& scenes]
  (let [scenes (with-offsets scenes)
        duration (reduce + (map :duration scenes))]
    (scene duration
           (fn [p]
             (let [t (* p duration)
                   current (or (some (fn [{:keys [start] :as scene}]
                                       (when (< t (+ start (:duration scene)))
                                         scene))
                                     scenes)
                               (last scenes))]
               (when current
                 (at current (- t (:start current)))))))))

(defn parallel
  "The scenes at the same time, as a fragment in their order. A shorter
  scene stays at its end while longer ones go on."
  [& scenes]
  (let [duration (reduce max 0 (map :duration scenes))]
    (scene duration
           (fn [p]
             (let [t (* p duration)]
               (for [scene scenes]
                 (at scene t)))))))

(defn repeat
  "The scene `n` times in a row."
  [n scene]
  (let [n (max 1 (long n))]
    (assoc scene
           :duration (* n (:duration scene))
           :render (fn [p]
                     (frame scene (if (>= p 1)
                                    1
                                    (* (mod (double p) (/ 1.0 n)) n)))))))

(defn offset
  "The scene starting `seconds` later: still (nothing) before it."
  [seconds scene]
  (sequence (still seconds nil) scene))

(defn window
  "The progress of the part of a scene between `start` and `end` seconds,
  given the scene's progress `p` and `duration`: 0 before, 1 after. For a
  render function that animates several things at different times."
  [p duration start end]
  (let [t (* (double p) duration)]
    (if (<= end start)
      (if (< t start) 0 1)
      (max 0 (min 1 (/ (- t start) (- end start)))))))

(comment
  (def title (scene 2 (fn [p] [:h1 {:style {:opacity p}} "Hello"])))
  (at (sequence title (still 1 [:p "and then"])) 2.5)
  (at (parallel title (still 3 [:footer "always"])) 1)
  )
