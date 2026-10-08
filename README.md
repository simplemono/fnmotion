# fnmotion

A frame is a pure function of time. A video is a value, the player is a
loop, and the scrubber is the point: whatever you build, you can drag
through it and see every moment at once.

```clojure
(require '[fnmotion.core :as fm]
         '[fnmotion.timeline :as tl])

(def title
  (tl/scene 2 (fn [p]
                [:h1 {:style {:opacity p
                              :transform (str "scale(" (fm/lerp 0.8 1 (fm/cubic-out p)) ")")}}
                 "Hello"])))

(def video
  (tl/sequence title (tl/still 3 [:p "and then"])))

(tl/at video 1)     ; => [:h1 {:style {:opacity 0.5, ...}} "Hello"]
(tl/at video 2.5)   ; => [:p "and then"]
```

That is the whole model, the one Remotion and HyperFrames use, in hiccup.
A scene is `{:duration seconds :render (fn [p] hiccup)}` with `p` the
progress 0..1. Because it is a function, the same scene feeds a browser
player, a thumbnail at any time, a test, and a frame-by-frame render.

The pure parts are `.cljc` with no dependencies: they run on the JVM, on
babashka and in the browser, and a REPL can try a frame. The player needs
[Replicant](https://replicant.fun) in the browser.

## The namespaces

**`fnmotion.core`**, numbers from time: `progress` of a time within a
window, `lerp`, `interpolate` (a time mapped from an input range to an
output range, clamped, with an optional easing), the easings `quad-in`,
`cubic-in`, `quint-in`, their `-out` and `-in-out` forms, `ease-out` and
`ease-in-out` to make more, `spring`, `oscillate`, `reverse`, `delay`,
`limit`, and `px` and `pct` for CSS.

```clojure
(fm/interpolate t [2 2.5] [0 100] {:easing fm/cubic-out})
(fm/spring (fm/progress t 0 1.2))
```

**`fnmotion.timeline`**, scenes: `scene`, `still`, `sequence` (one after
the other), `parallel` (at the same time, a fragment), `repeat`, `offset`,
`at` (the frame at a time in seconds), `frame` (at a progress) and
`window` (the progress of a part of a scene, for a render function that
animates several things at different times).

**`fnmotion.captions`**, from a transcript with word timings
(`{:words [{:start :end :text} ...]}`, the shape speech-to-text returns)
to captions a viewer can read: `fill-breaks` extends each word into the
pause after it so a caption does not flicker, `blocks` halves the words
until every block fits `max-chars`, so the blocks come out even and none
flashes by, `captions` does both, `at` is the block on at a time, `word-at`
the spoken word for karaoke-style highlighting.

```clojure
(def blocks (captions/captions transcript {:max-chars 24}))
(captions/text (captions/at blocks 1.2))   ; => "Welcome to the show,"
```

**`fnmotion.player`** (browser): `mount!` renders a scene into an element
with play, pause, a scrubber and the time, re-rendering with Replicant
only when the time changed. `make`, `play!`, `pause!`, `seek!` and
`controls` are the pieces for an app with its own layout, `sync-media!`
keeps a `<video>` or `<audio>` in step.

```clojure
(player/mount! {:element "#app" :scene video :media "#voice"})
```

## The example

A ten-second short: a title that springs in, captions from word timings
with the spoken word highlighted, a progress bar, a closing card.

```bash
cd example
clojure -M -m shadow.cljs.devtools.cli release browser   # needs a JVM
python3 -m http.server 8000 --directory public           # http://localhost:8000
```

## Install

```clojure
io.github.simplemono/fnmotion {:git/url "https://github.com/simplemono/fnmotion.git"
                               :git/sha "<sha>"}
```

## Tests

```bash
bb test                 # the pure parts on babashka
clojure -M:test         # the same on the JVM
```

## Where it came from

A 2022 prototype at SimpleValue for animated shorts from podcast audio:
captions, a pulsing speaker ring from the audio level, a ranking that
counts up. The idea survived as it was; the code was rewritten for
Replicant and babashka, with the caption algorithm kept.
