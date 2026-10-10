(ns fnmotion.rframes
  "The player of a motion design in an rframes app, on the server.

  The app keeps everything in one atom: the design data under `:design`,
  the time on screen under `:motion/t`, whether it plays under
  `:motion/playing?`. The frame is a pure function of the design and the
  time, `(frame-fn design t)`, hiccup. `player` renders the frame at the
  current time with play, a scrubber and the clock; `commands` are the
  entries the controls send; while it plays, a tick loop advances the
  time with the wall clock and pushes a frame to every connection after
  each step, past rframes' coalescing, for sixty a second.

      (def db (atom {:design {:title \"Hi\"} :motion/t 0.0}))
      (defn frame [design t] [:h1 {:style {:opacity (fm/progress t 0 1)}} (:title design)])
      (fnmotion.rframes/player db {:duration 6 :frame frame})
      (fnmotion.rframes/commands db {:duration 6})   ; concat into the register

  A control in a frame is a control anywhere: `:design/set` writes a
  value into the design (a number stays a number) and the frame renders
  again. `watch-design!` keeps the design in a file across restarts.

  The classes match the browser player of `fnmotion.player`; `css` has
  the rules for an app without a stylesheet of its own."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [rframes.command :as command]
            [rframes.sse :as sse]))

(def tick-ms
  "The pause between two pushed frames, for sixty a second."
  16)

;;; The frame

(defn data-only
  "A frame may hold a function by mistake; a function would drop the whole
  frame. It becomes nothing."
  [hiccup]
  (walk/postwalk (fn [x] (if (fn? x) nil x)) hiccup))

(defn frame
  "The frame at `t`, or the error it raised as a paragraph."
  [frame-fn design t]
  (try
    (data-only (frame-fn design t))
    (catch Throwable e
      [:p.error (str "The frame at " (format "%.1f" (double t)) " s failed: " (ex-message e))])))

;;; Playback

(defn- duration-of
  [duration]
  (double (if (fn? duration) (duration) duration)))

(defn- tick-loop!
  [db duration]
  (let [started (System/nanoTime)
        t0 (double (:motion/t @db 0.0))]
    (loop []
      (when (:motion/playing? @db)
        (let [duration (duration-of duration)
              t (+ t0 (/ (- (System/nanoTime) started) 1e9))]
          (if (>= t duration)
            (swap! db assoc :motion/t duration :motion/playing? false)
            (do (swap! db assoc :motion/t t)
                (sse/refresh-all!)
                (Thread/sleep tick-ms)
                (recur))))))))

(defn toggle!
  "Plays or pauses; playing from the end starts over."
  [db duration]
  (let [playing? (not (:motion/playing? @db))
        t (double (:motion/t @db 0.0))]
    (swap! db assoc
           :motion/playing? playing?
           :motion/t (if (and playing? (>= t (duration-of duration))) 0.0 t))
    (when playing?
      (future (tick-loop! db duration)))
    playing?))

(defn seek!
  "Pauses at the scrubber's `value`, 0..1000 of the duration, and pushes
  the frame at once: the scrubber should not wait for the coalescing."
  [db duration value]
  (swap! db assoc
         :motion/playing? false
         :motion/t (* (duration-of duration) (/ (double value) 1000.0)))
  (sse/refresh-all!))

(defn stop!
  "Pauses at the start."
  [db]
  (swap! db assoc :motion/playing? false :motion/t 0.0))

;;; The design data

(defn- coerce
  "The value of a control: a number stays a number."
  [value]
  (if (string? value)
    (if-let [number (parse-double value)]
      (if (str/includes? value ".") number (long number))
      value)
    value))

(defn design-set!
  "Writes `value` at `path` into the design."
  [db path value]
  (swap! db assoc-in (into [:design] (map #(if (string? %) (keyword %) %)) path) (coerce value)))

(defn watch-design!
  "Writes the design into `file` whenever it changes, and reads it back
  first when the file exists: the design survives a restart."
  [db file]
  (let [file (io/file file)]
    (when (.isFile file)
      (swap! db assoc :design (edn/read-string (slurp file))))
    (add-watch db ::design
               (fn [_ _ old new]
                 (when (not= (:design old) (:design new))
                   (io/make-parents file)
                   (spit file (pr-str (:design new))))))
    db))

;;; The entries and the view

(defn commands
  "The commands of the controls: `:motion/toggle`, `:motion/seek` with
  `{:value 0..1000}`, `:design/set` with `{:path [...] :value ...}`.
  `duration` is seconds or a function of no arguments."
  [db {:keys [duration]}]
  [{:command/kind :motion/toggle
    :command/fn (fn [w]
                  (toggle! db duration)
                  (command/accepted w))}
   {:command/kind :motion/seek
    :command/fn (fn [w]
                  (seek! db duration (or (parse-double (str (:value (command/command-data w)))) 0.0))
                  (command/accepted w))}
   {:command/kind :design/set
    :command/fn (fn [w]
                  (let [{:keys [path value]} (command/command-data w)]
                    (if (sequential? path)
                      (do (design-set! db path value)
                          (command/accepted w))
                      (command/rejected w "A path is a vector."))))}])

(defn setter
  "The `:on` map of a control that writes its value at `path`:
  `[:input {:type \"range\" :value size :on (setter [:size])}]`."
  [path]
  {:input [[:data/command {:command/kind :design/set
                           :command/data {:path path
                                          :value :event/target.value}}]]})

(defn- timestamp
  [seconds]
  (let [seconds (double (or seconds 0))]
    (format "%d:%04.1f" (long (quot seconds 60)) (rem seconds 60.0))))

(defn controls
  "Play or pause, the scrubber, the clock."
  [{:keys [motion/t motion/playing?] :or {t 0.0}} duration]
  (let [duration (duration-of duration)
        t (min (double t) duration)]
    [:div.fnmotion-controls
     [:button {:on {:click [[:data/command {:command/kind :motion/toggle}]]}}
      (if playing? "Pause" "Play")]
     [:input {:type "range"
              :min 0
              :max 1000
              :value (long (* 1000 (/ t (max duration 0.001))))
              :on {:input [[:data/command {:command/kind :motion/seek
                                           :command/data {:value :event/target.value}}]]}}]
     [:span.fnmotion-time (str (timestamp t) " / " (timestamp duration))]]))

(defn player
  "The frame at the current time over the controls. `:frame` is the
  function of design and time, `:duration` seconds or a function."
  [{:keys [design motion/t] :or {t 0.0} :as db-value} {:keys [frame duration] :as opts}]
  (let [duration (duration-of duration)]
    [:div.motion
     [:div.motion-frame (fnmotion.rframes/frame frame design (min (double t) duration))]
     (controls db-value duration)]))

(def css
  "The rules of the player for an app without a stylesheet of its own."
  ".motion { display: flex; flex-direction: column; gap: 12px; }
.motion-frame { position: relative; min-height: 120px; }
.fnmotion-controls { display: flex; align-items: center; gap: 10px; }
.fnmotion-controls input[type=range] { flex: 1; }
.fnmotion-controls button { padding: 6px 14px; border: 1px solid #ccc; border-radius: 6px; background: #fff; cursor: pointer; font: inherit; }
.fnmotion-time { font-variant-numeric: tabular-nums; color: #555; min-width: 7em; text-align: right; }
.error { color: #b00020; }")
