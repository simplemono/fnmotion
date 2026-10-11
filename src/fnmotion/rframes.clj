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

(def design-poll-ms
  "How often `watch-design!` looks at the file for an edit from outside."
  500)

(defn- read-design-file!
  "The design in the file into the atom and onto the page, when the file
  holds another design than the atom. True when the file could be read:
  a write in progress is empty or does not parse, and the next look sees
  the whole file."
  [db file]
  (try
    (let [design (edn/read-string (slurp file))]
      (when (some? design)
        (when (not= design (:design @db))
          (swap! db assoc :design design)
          (sse/refresh-all!))
        true))
    (catch Exception _
      false)))

(defn watch-design!
  "Writes the design into `file` whenever it changes, reads it back first
  when the file exists, and reads it again when something else writes
  the file (an editor, an agent): the design survives a restart, and an
  edit of the file shows up on the page."
  [db file]
  (let [file (io/file file)]
    (when (.isFile file)
      (swap! db assoc :design (edn/read-string (slurp file))))
    (add-watch db ::design
               (fn [_ _ old new]
                 (when (not= (:design old) (:design new))
                   (io/make-parents file)
                   (spit file (pr-str (:design new))))))
    (future
      (loop [seen (.lastModified file)]
        (Thread/sleep design-poll-ms)
        (let [now (.lastModified file)]
          (recur (if (or (= seen now) (read-design-file! db file))
                   now
                   seen)))))
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

(defn- stage
  "The frame in a box of the size of the video, scaled to the width of
  the pane by `script`."
  [[w h] frame]
  [:div.motion-frame {:data-width w
                      :data-height h
                      :style {:aspect-ratio (str w " / " h)
                              :max-width (str "calc(70vh * " w " / " h ")")}}
   [:div.motion-stage {:style {:width (str w "px")
                               :height (str h "px")
                               :transform "scale(var(--motion-scale, 1))"
                               :transform-origin "top left"}}
    frame]])

(defn audio-tracks
  "The audio tracks of the design as elements `script` keeps in step with
  the time. A track is `{:src :start :duration :media-start :volume
  :loop?}`, see fnmotion.render."
  [tracks]
  (when (seq tracks)
    [:div.motion-audio
     (for [{:keys [src start duration media-start volume loop?]} tracks]
       [:audio {:replicant/key src
                :src src
                :preload "auto"
                :data-start (or start 0)
                :data-duration duration
                :data-media-start (or media-start 0)
                :data-volume (or volume 1)
                :loop (boolean loop?)}])]))

(defn player
  "The frame at the current time over the controls. `:frame` is the
  function of design and time, `:duration` seconds or a function, `:size`
  the width and height of the video in pixels, when the design has one,
  `:audio` its tracks, the `:audio` of the design when absent."
  [{:keys [design motion/t motion/playing?] :or {t 0.0} :as db-value} {:keys [frame duration size audio]}]
  (let [duration (duration-of duration)
        t (min (double t) duration)
        the-frame (fnmotion.rframes/frame frame design t)]
    [:div.motion {:data-t t
                  :data-playing (str (boolean playing?))}
     (if size
       (stage size the-frame)
       [:div.motion-frame the-frame])
     (audio-tracks (or audio (:audio design)))
     (controls db-value duration)]))

(def script
  "For a <script> in the head of the page: scales the stage to the width
  of the pane, and keeps the audio tracks in step with the time, playing
  while the design plays, at the position of the scrubber otherwise."
  "(function () {
  function fit() {
    var frame = document.querySelector('.motion-frame[data-width]');
    if (!frame) return;
    var width = parseFloat(frame.dataset.width);
    if (width > 0 && frame.clientWidth > 0) {
      document.documentElement.style.setProperty('--motion-scale', frame.clientWidth / width);
    }
  }
  function sync() {
    var motion = document.querySelector('.motion[data-t]');
    if (!motion) return;
    var t = parseFloat(motion.dataset.t) || 0;
    var playing = motion.dataset.playing === 'true';
    Array.prototype.forEach.call(motion.querySelectorAll('audio[data-start]'), function (audio) {
      var start = parseFloat(audio.dataset.start) || 0;
      var duration = parseFloat(audio.dataset.duration);
      var mediaStart = parseFloat(audio.dataset.mediaStart) || 0;
      var volume = parseFloat(audio.dataset.volume);
      var local = t - start;
      var on = local >= 0 && (isNaN(duration) || local < duration);
      var want = mediaStart + local;
      if (audio.loop && audio.duration > mediaStart) {
        want = mediaStart + (local % (audio.duration - mediaStart));
      }
      audio.volume = isNaN(volume) ? 1 : Math.max(0, Math.min(1, volume));
      if (playing && on) {
        if (Math.abs(audio.currentTime - want) > 0.3) audio.currentTime = want;
        if (audio.paused) audio.play().catch(function () {});
      } else {
        if (!audio.paused) audio.pause();
        if (want >= 0 && Math.abs(audio.currentTime - want) > 0.3) audio.currentTime = want;
      }
    });
  }
  new MutationObserver(function () { fit(); sync(); })
    .observe(document.documentElement, {subtree: true, childList: true, attributes: true,
                                         attributeFilter: ['data-t', 'data-playing', 'data-width']});
  window.addEventListener('resize', fit);
  document.addEventListener('DOMContentLoaded', function () { fit(); sync(); });
})();")

(def css
  "The rules of the player for an app without a stylesheet of its own."
  ".motion { display: flex; flex-direction: column; gap: 12px; }
.motion-frame { position: relative; width: 100%; overflow: hidden; }
.motion-frame:not([data-width]) { min-height: 120px; }
.motion-stage { position: relative; overflow: hidden; }
.motion-audio { display: none; }
.fnmotion-controls { display: flex; align-items: center; gap: 10px; }
.fnmotion-controls input[type=range] { flex: 1; }
.fnmotion-controls button { padding: 6px 14px; border: 1px solid #ccc; border-radius: 6px; background: #fff; cursor: pointer; font: inherit; }
.fnmotion-time { font-variant-numeric: tabular-nums; color: #555; min-width: 7em; text-align: right; }
.error { color: #b00020; }")
