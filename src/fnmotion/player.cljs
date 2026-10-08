(ns fnmotion.player
  "Plays a scene in the browser: a requestAnimationFrame loop that renders
  the frame at the current time with Replicant, only when the time
  changed, with play, pause and seek, a scrubber, and media elements kept
  in step. The player is an atom

      {:t 0 :duration 10 :playing? false}

  and the controls are hiccup with Replicant handlers, so an app puts
  them anywhere in its own view."
  (:require [fnmotion.timeline :as timeline]
            [replicant.dom :as r]))

(defn make
  "A player for a scene of `duration` seconds. `on-change` is called with
  the player state after every change, e.g. to render."
  [{:keys [duration on-change]}]
  (let [player (atom {:t 0
                      :duration duration
                      :playing? false})]
    (when on-change
      (add-watch player ::on-change (fn [_ _ old new]
                                      (when (not= old new)
                                        (on-change new)))))
    player))

(defn seek!
  [player t]
  (swap! player (fn [{:keys [duration] :as state}]
                  (assoc state :t (max 0 (min duration t))))))

(defn pause!
  [player]
  (swap! player assoc :playing? false))

(defn- tick!
  "One frame of playback: advances `t` by the real time that passed."
  [player started-at started-t]
  (fn callback [now]
    (let [{:keys [duration playing?]} @player
          t (+ started-t (/ (- now started-at) 1000))]
      (when playing?
        (if (< t duration)
          (do (swap! player assoc :t t)
              (js/requestAnimationFrame callback))
          (swap! player assoc :t duration :playing? false))))))

(defn play!
  "Plays from the current time, from the start when it is at the end."
  [player]
  (let [{:keys [t duration playing?]} @player]
    (when-not playing?
      (let [t (if (>= t duration) 0 t)]
        (swap! player assoc :t t :playing? true)
        (js/requestAnimationFrame (tick! player (js/performance.now) t))))))

(defn toggle!
  [player]
  (if (:playing? @player)
    (pause! player)
    (play! player)))

(defn sync-media!
  "Keeps a <video> or <audio> element in step with the player: seeks it
  to `t` and plays or pauses it with the player."
  [^js element {:keys [t playing?]}]
  (when element
    (when (< 0.1 (Math/abs (- (.-currentTime element) t)))
      (set! (.-currentTime element) t))
    (if playing?
      (when (.-paused element)
        (.play element))
      (when-not (.-paused element)
        (.pause element)))))

(defn- timestamp
  [seconds]
  (let [tenths (Math/floor (* 10 (max 0 seconds)))
        minutes (quot tenths 600)
        rest-tenths (- tenths (* 600 minutes))]
    (str minutes ":" (when (< rest-tenths 100) "0") (.toFixed (/ rest-tenths 10) 1))))

(defn controls
  "Play/pause, a scrubber over the whole scene and the time, as hiccup for
  Replicant with handlers that drive the player."
  [player]
  (let [{:keys [t duration playing?]} @player]
    [:div.fnmotion-controls
     [:button {:on {:click (fn [_] (toggle! player))}}
      (if playing? "Pause" "Play")]
     [:input {:type "range"
              :min 0
              :max 1000
              :value (if (pos? duration) (* 1000 (/ t duration)) 0)
              :on {:input (fn [^js event]
                            (pause! player)
                            (seek! player (* duration (/ (js/parseFloat (.. event -target -value)) 1000))))}}]
     [:span.fnmotion-time (str (timestamp t) " / " (timestamp duration))]]))

(defn mount!
  "Renders `scene` into `element` (a node or a selector) with the controls
  under it and returns the player. `media` is an optional selector of a
  <video> or <audio> kept in step. `wrap` (optional) is a function from
  the frame hiccup and the controls hiccup to the page hiccup."
  [{:keys [element scene media wrap]
    :or {wrap (fn [frame controls]
                [:div.fnmotion
                 [:div.fnmotion-frame frame]
                 controls])}}]
  (let [node (if (string? element)
               (js/document.querySelector element)
               element)
        player (make {:duration (:duration scene)})
        render! (fn [{:keys [t] :as state}]
                  (r/render node (wrap (timeline/at scene t) (controls player)))
                  (when media
                    (sync-media! (js/document.querySelector media) state)))]
    (add-watch player ::render (fn [_ _ old new]
                                 (when (not= old new)
                                   (render! new))))
    (render! @player)
    player))
