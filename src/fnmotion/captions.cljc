(ns fnmotion.captions
  "Captions from a transcript with word timings: the words grouped into
  blocks a viewer can read, and the block or word that is on at a time.

  A transcript is `{:words [{:start s :end s :text \"...\"} ...]}`, the
  shape speech-to-text services return."
  (:require [clojure.string :as str]))

(defn fill-breaks
  "The words with each one extended into the pause after it, by at most
  `max-break` seconds, so that a caption does not flicker off between two
  words."
  [max-break words]
  (map (fn [[word next-word]]
         (if next-word
           (let [pause (- (:start next-word 0) (:end word 0))]
             (update word :end + (min (Math/abs pause) max-break)))
           word))
       (partition-all 2 1 words)))

(defn- chars-of
  [words]
  (reduce + (map (comp count :text) words)))

(defn blocks
  "The words split into blocks of at most `max-chars` characters each,
  halved until they fit, so that the blocks come out even and no block
  flashes by too fast to read. Each block is `{:start :end :words}`."
  [max-chars words]
  (let [words (vec words)]
    (if (and (> (count words) 1)
             (> (chars-of words) max-chars))
      (let [[a b] (split-at (Math/ceil (/ (count words) 2)) words)]
        (concat (blocks max-chars a)
                (blocks max-chars b)))
      (when (seq words)
        [{:start (:start (first words))
          :end (:end (last words))
          :words words}]))))

(defn text
  "The text of a block, or of any sequence of words."
  [words-or-block]
  (str/join " " (map :text (if (map? words-or-block)
                             (:words words-or-block)
                             words-or-block))))

(defn captions
  "The caption blocks of a transcript: the pauses filled up to
  `:max-break` seconds (0.2), the words grouped into blocks of at most
  `:max-chars` (24)."
  ([transcript]
   (captions transcript nil))
  ([{:keys [words]} {:keys [max-break max-chars]
                     :or {max-break 0.2
                          max-chars 24}}]
   (vec (blocks max-chars (fill-breaks max-break words)))))

(defn at
  "The block of `blocks` that is on at time `t`, or nil."
  [blocks t]
  (some (fn [{:keys [start end] :as block}]
          (when (and (<= start t) (< t end))
            block))
        blocks))

(defn word-at
  "The word of `words` that is spoken at time `t`, or nil. For
  karaoke-style highlighting of the current word within a block."
  [words t]
  (some (fn [{:keys [start end] :as word}]
          (when (and (<= start t) (< t end))
            word))
        words))

(comment
  (def transcript
    {:words [{:start 0.0 :end 0.3 :text "Welcome"}
             {:start 0.3 :end 0.5 :text "to"}
             {:start 0.6 :end 1.0 :text "the"}
             {:start 1.0 :end 1.6 :text "show,"}
             {:start 2.0 :end 2.4 :text "I'm"}
             {:start 2.4 :end 2.9 :text "Max."}]})
  (captions transcript {:max-chars 12})
  (text (at (captions transcript {:max-chars 12}) 1.2))
  )
