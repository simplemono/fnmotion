(ns fnmotion.core
  "A frame is a pure function of time. These are the small functions that
  turn a time into the numbers a frame needs: progress within a window,
  easings, interpolation. Everything here is a plain function of numbers,
  so it runs on the JVM, on babashka and in the browser, and a REPL can
  try it."
  (:refer-clojure :exclude [reverse delay]))

(defn clamp
  "`x` limited to [lo hi]."
  [x lo hi]
  (max lo (min hi x)))

(defn progress
  "Where `t` is within the window from `start` to `end`, 0 before it, 1
  after it, linear in between."
  [t start end]
  (if (<= end start)
    (if (< t start) 0 1)
    (clamp (/ (double (- t start)) (- end start)) 0 1)))

(defn lerp
  "The value `p` (0..1) of the way from `a` to `b`."
  [a b p]
  (+ a (* (- b a) (double p))))

;;; Easings: progress 0..1 -> 0..1

(defn linear
  [p]
  p)

(defn quad-in
  [p]
  (* p p))

(defn cubic-in
  [p]
  (* p p p))

(defn quint-in
  "The prototype called this cubic; it is p^5, a hard start."
  [p]
  (Math/pow p 5))

(defn ease-out
  "The ease-out variant of an ease-in: the same curve, mirrored."
  [ease-in]
  (fn [p]
    (- 1 (ease-in (- 1 p)))))

(defn ease-in-out
  "Ease-in for the first half, ease-out for the second."
  [ease-in]
  (let [out (ease-out ease-in)]
    (fn [p]
      (if (< p 0.5)
        (/ (ease-in (* 2 p)) 2)
        (+ 0.5 (/ (out (- (* 2 p) 1)) 2))))))

(def quad-out
  (ease-out quad-in))

(def cubic-out
  (ease-out cubic-in))

(def quad-in-out
  (ease-in-out quad-in))

(def cubic-in-out
  (ease-in-out cubic-in))

(defn oscillate
  "One full sine wave over the progress, -1..1."
  [p]
  (Math/sin (* p 2 Math/PI)))

(defn reverse
  [p]
  (- 1 p))

(defn delay
  "The progress with its first `fraction` spent waiting: 0 until then,
  then 0..1 over the rest."
  [p fraction]
  (if (< p fraction)
    0
    (/ (double (- p fraction)) (- 1 fraction))))

(defn limit
  "The progress squeezed into the first `fraction`: 0..1 over it, then 1."
  [p fraction]
  (min (/ (double p) fraction) 1))

(defn spring
  "A damped spring settling at 1, for a value that overshoots and comes
  back. `stiffness` is how many wobbles, `damping` how fast they die."
  ([p]
   (spring p 8 6))
  ([p stiffness damping]
   (if (>= p 1)
     1
     (- 1 (* (Math/exp (- (* damping p)))
             (Math/cos (* stiffness p)))))))

;;; Interpolation, the Remotion-style workhorse

(defn interpolate
  "`t` mapped from the input range to the output range, clamped to it,
  with an optional easing over the input range.

      (interpolate t [0 1] [0 100])
      (interpolate t [2 2.5] [0 1] {:easing cubic-out})"
  ([t in out]
   (interpolate t in out nil))
  ([t [in0 in1] [out0 out1] {:keys [easing] :or {easing linear}}]
   (lerp out0 out1 (easing (progress t in0 in1)))))

(defn- css-number
  "A number for CSS: a whole value without the trailing .0 the JVM
  prints, otherwise as it is."
  [n]
  (if (== n (Math/floor n))
    (str (long n))
    (str n)))

(defn px
  "A number as a CSS pixel length."
  [n]
  (str (css-number n) "px"))

(defn pct
  "A fraction 0..1 as a CSS percentage."
  [p]
  (str (css-number (* 100 (double p))) "%"))

(comment
  (interpolate 1.25 [1 2] [0 100])
  (interpolate 1.25 [1 2] [0 100] {:easing cubic-out})
  (map #(spring (/ % 10)) (range 11))
  )
