(ns fnmotion.render
  "A motion design as a video file: every frame of the design as HTML, a
  headless Chrome shows them one after the other at the size of the video
  and screenshots each, ffmpeg encodes the screenshots and mixes the
  audio tracks of the design in.

      (render! {:frame design-view :design design :duration 6 :fps 30
                :size [1080 1920] :head styles :dir \".\"
                :audio [{:src \"music.mp3\" :volume 0.6 :fade-out 1}]
                :out \"../hello.mp4\"})

  Chrome is driven over its DevTools protocol, the way HyperFrames does
  it: a page with a stage of the video's size and a function that puts
  the HTML of a frame into the stage, then `Page.captureScreenshot` per
  frame, piped into ffmpeg in order as they come. A few Chromes work
  side by side (`:workers`), each on every nth frame. Nothing but the
  video reaches the disk. The page is a file in `:dir`, so a relative `src` in a frame
  finds the file it finds on the page of the design; `:head` is what the
  head of that page has, the styles and the fonts.

  An audio track is a map: `:src` the file, relative to `:dir`; `:start`
  when it begins in the video, seconds, 0; `:duration` how long it plays,
  to the end of the video; `:media-start` where in the file it begins,
  0; `:volume` 0..1, 1; `:fade-in` and `:fade-out` in seconds, 0; `:loop?`
  whether the file starts over when it ends, false. The words of
  HyperFrames' data-start, data-duration, data-media-start and
  data-volume. The tracks are mixed; the video sets the length."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.http-client.websocket :as ws]
            [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.string :as str]
            [fnmotion.html :as html]))

;;; The frames

(defn frame-times
  "The times of the frames of a video of `duration` seconds at `fps`."
  [duration fps]
  (let [n (Math/round (* (double duration) fps))]
    (mapv #(/ % (double fps)) (range n))))

(def ^:private show-script
  "The function on the page that puts a frame into the stage and settles
  when its fonts and images are there, laid out. The screenshot itself
  draws the frame; waiting for an animation frame here would hang in a
  headless Chrome that thinks its page is hidden."
  (str "window.__fnmotion = {show: function (html) {"
       " var stage = document.getElementById('stage');"
       " stage.innerHTML = html;"
       " var images = Array.prototype.map.call(stage.querySelectorAll('img'), function (img) { return img.decode().catch(function () {}); });"
       " return Promise.all([document.fonts.ready].concat(images)).then(function () {"
       "  void stage.offsetHeight;"
       "  return true;"
       " });"
       "}};"))

(defn page-html
  "The page Chrome shows: the head of the design's page, a stage of the
  size of the video, the function that puts a frame into it."
  [{:keys [size head background]}]
  (let [[w h] size]
    (str "<!doctype html><html><head><meta charset=\"utf-8\">"
         head
         "<style>html, body { margin: 0; padding: 0; overflow: hidden; background: " (or background "#fff") "; }"
         " #stage { position: relative; width: " w "px; height: " h "px; overflow: hidden; }"
         " #stage * { animation: none !important; transition: none !important; }</style>"
         "</head><body><div id=\"stage\" class=\"motion-stage\"></div>"
         "<script>" show-script "</script></body></html>")))

;;; Chrome

(def chrome-names
  ["google-chrome" "google-chrome-stable" "chromium" "chromium-browser" "chrome"])

(defn find-chrome
  "`CHROME` in the environment, else the first of the usual names on the
  PATH, else nil."
  []
  (or (not-empty (System/getenv "CHROME"))
      (some #(some-> (fs/which %) str) chrome-names)))

(defn- devtools-port
  "The port Chrome wrote into its profile, within twenty seconds."
  [profile process]
  (let [file (fs/file profile "DevToolsActivePort")]
    (loop [waited 0]
      (let [lines (when (fs/exists? file)
                    (str/split-lines (slurp (str file))))]
        (cond
          (= 2 (count lines))
          (parse-long (first lines))

          (not (.isAlive (:proc process)))
          (throw (ex-info (str "Chrome exited: " (str/trim (slurp (str (fs/file profile "chrome.log"))))) {}))

          (< 20000 waited)
          (throw (ex-info "Chrome did not open its DevTools port in 20 s" {:profile (str profile)}))

          :else
          (do (Thread/sleep 100)
              (recur (+ waited 100))))))))

(defn- start-chrome!
  "A headless Chrome of its own, with a window no smaller than a desktop
  one: Chrome takes a tiny window for hidden and paints no frame in it,
  and a screenshot waits for a frame. The page itself gets the size of
  the video from `prepare-page!`."
  [chrome [w h]]
  (let [[w h] [(max (long w) 1024) (max (long h) 768)]
        profile (fs/create-temp-dir {:prefix "fnmotion-chrome"})
        log (fs/file profile "chrome.log")
        process (process/process [chrome
                                  "--headless=new" "--no-sandbox" "--disable-gpu" "--disable-dev-shm-usage"
                                  "--hide-scrollbars" "--no-first-run" "--disable-extensions" "--mute-audio"
                                  "--disable-background-networking" "--force-device-scale-factor=1"
                                  "--force-color-profile=srgb"
                                  ;; Every page stays awake, as with Puppeteer.
                                  "--disable-renderer-backgrounding"
                                  "--disable-backgrounding-occluded-windows"
                                  "--disable-background-timer-throttling"
                                  "--disable-hang-monitor"
                                  (str "--window-size=" w "," h)
                                  "--remote-debugging-port=0"
                                  (str "--user-data-dir=" profile)
                                  "about:blank"]
                                 {:out log :err log})]
    {:process process
     :profile profile
     :port (devtools-port profile process)}))

(defn- stop-chrome!
  "Ends Chrome and removes its profile once it is gone."
  [process profile]
  (process/destroy-tree process)
  (when-not (.waitFor (:proc process) 10 java.util.concurrent.TimeUnit/SECONDS)
    (.destroyForcibly (:proc process))
    (.waitFor (:proc process) 5 java.util.concurrent.TimeUnit/SECONDS))
  (try
    (fs/delete-tree profile)
    (catch Exception _
      ;; Chrome wrote into it after it said goodbye; the next look wins.
      (Thread/sleep 500)
      (fs/delete-tree profile {:force true}))))

(defn- page-url
  "The DevTools socket of the page Chrome opened."
  [port]
  (let [targets (json/parse-string (:body (http/get (str "http://127.0.0.1:" port "/json/list"))) true)]
    (or (some #(when (= "page" (:type %)) (:webSocketDebuggerUrl %)) targets)
        (throw (ex-info "Chrome opened no page" {:targets targets})))))

;;; The DevTools protocol

(defn- connect!
  [url]
  (let [pending (atom {})
        buffer (StringBuilder.)
        socket (ws/websocket {:uri url
                              :on-message (fn [_ data last?]
                                            (.append buffer (str data))
                                            (when last?
                                              (let [message (json/parse-string (str buffer) true)]
                                                (.setLength buffer 0)
                                                (when-let [p (get @pending (:id message))]
                                                  (deliver p message)))))
                              :on-error (fn [_ e]
                                          (doseq [p (vals @pending)]
                                            (deliver p {:error {:message (str e)}})))})]
    {:socket socket
     :pending pending
     :ids (atom 0)}))

(defn- call!
  "One method of the protocol: its result, or the error it answered."
  [{:keys [socket pending ids]} method params]
  (let [id (swap! ids inc)
        p (promise)]
    (swap! pending assoc id p)
    (ws/send! socket (json/generate-string {:id id :method method :params (or params {})}))
    (let [{:keys [result error] :as message} (deref p 60000 nil)]
      (swap! pending dissoc id)
      (cond
        (nil? message) (throw (ex-info (str method " did not answer in 60 s") {}))
        error (throw (ex-info (str method ": " (:message error)) {:error error}))
        :else result))))

(defn- evaluate!
  [cdp expression]
  (let [{:keys [result exceptionDetails]} (call! cdp "Runtime.evaluate" {:expression expression
                                                                         :awaitPromise true
                                                                         :returnByValue true})]
    (when exceptionDetails
      (throw (ex-info (str "The page failed: " (or (get-in exceptionDetails [:exception :description])
                                                   (:text exceptionDetails)))
                      {})))
    (:value result)))

(defn- wait-ready!
  [cdp]
  (loop [waited 0]
    (when-not (true? (evaluate! cdp "document.readyState === 'complete' && !!window.__fnmotion"))
      (when (< 30000 waited)
        (throw (ex-info "The render page did not load in 30 s" {})))
      (Thread/sleep 50)
      (recur (+ waited 50)))))

(defn- screenshot!
  "The viewport as PNG bytes."
  [cdp]
  (.decode (java.util.Base64/getDecoder)
           ^String (:data (call! cdp "Page.captureScreenshot" {:format "png"}))))

;;; ffmpeg

(defn- num
  "A number in a filter: 1.5 or 2, never 1,5."
  [x]
  (let [v (/ (Math/round (* 1000.0 (double x))) 1000.0)]
    (if (== v (Math/floor v))
      (str (long v))
      (str v))))

(defn track-filter
  "The filter of one audio track, the input `i`: the part of the file that
  plays, at its volume, faded, delayed to its start; out as [a<i>]."
  [i {:keys [start duration media-start volume fade-in fade-out]} video-duration]
  (let [start (double (or start 0))
        plays (max 0.0 (min (double (or duration (- video-duration start)))
                            (- video-duration start)))
        volume (double (or volume 1))
        fade-in (double (or fade-in 0))
        fade-out (double (or fade-out 0))
        steps (cond-> [(str "atrim=start=" (num (or media-start 0)) ":duration=" (num plays))
                       "asetpts=PTS-STARTPTS"]
                (not= 1.0 volume) (conj (str "volume=" (num volume)))
                (pos? fade-in) (conj (str "afade=t=in:st=0:d=" (num fade-in)))
                (pos? fade-out) (conj (str "afade=t=out:st=" (num (max 0.0 (- plays fade-out))) ":d=" (num fade-out)))
                (pos? start) (conj (str "adelay=" (Math/round (* 1000 start)) ":all=1")))]
    (str "[" i ":a]" (str/join "," steps) "[a" i "]")))

(defn audio-filter
  "The filter graph that mixes the tracks, the inputs 1..n, into [aout]."
  [tracks video-duration]
  (let [n (count tracks)]
    (str (str/join ";" (map-indexed (fn [k track] (track-filter (inc k) track video-duration)) tracks))
         ";"
         (apply str (map #(str "[a" (inc %) "]") (range n)))
         (if (= 1 n)
           "anull"
           (str "amix=inputs=" n ":duration=longest:normalize=0"))
         "[aout]")))

(defn ffmpeg-args
  "The command: PNG frames on stdin at `fps`, the audio tracks as inputs,
  H.264 and AAC in an MP4 of `duration` seconds."
  [{:keys [ffmpeg fps duration audio out]}]
  (-> [(or ffmpeg "ffmpeg") "-y" "-loglevel" "error" "-f" "image2pipe" "-framerate" (str fps) "-i" "-"]
      (into (mapcat (fn [{:keys [src loop?]}]
                      (cond-> []
                        loop? (into ["-stream_loop" "-1"])
                        true (into ["-i" (str src)])))
                    audio))
      (into (when (seq audio)
              ["-filter_complex" (audio-filter audio duration)
               "-map" "0:v" "-map" "[aout]" "-c:a" "aac" "-b:a" "192k" "-ar" "48000"]))
      (into ["-c:v" "libx264" "-pix_fmt" "yuv420p" "-crf" "18" "-preset" "medium"
             "-t" (num duration) "-movflags" "+faststart" (str out)])))

;;; The render

(defn default-workers
  "Half the processors, one to four: a Chrome per worker."
  []
  (max 1 (min 4 (quot (.availableProcessors (Runtime/getRuntime)) 2))))

(defn- prepare-page!
  "A page of the size of the video, with the render page loaded."
  [cdp [w h] page]
  (call! cdp "Emulation.setDeviceMetricsOverride" {:width w
                                                   :height h
                                                   :deviceScaleFactor 1
                                                   :mobile false})
  (call! cdp "Page.navigate" {:url (str (.toUri (fs/path page)))})
  (call! cdp "Page.bringToFront" {})
  (wait-ready! cdp)
  cdp)

(defn- work!
  "One worker: its frames, every `workers`th from `first-index`, shown and
  screenshot into their slots. It stays close behind the writer so that
  no more than a few screenshots wait in memory. An error goes into the
  slots it still owes, so that the writer sees it."
  [cdp {:keys [frame design times slots written workers]} first-index]
  (let [mine (range first-index (count times) workers)]
    (try
      (doseq [i mine]
        (while (> (- i @written) (* 2 workers))
          (Thread/sleep 5))
        (evaluate! cdp (str "window.__fnmotion.show("
                            (json/generate-string (html/html (frame design (nth times i))))
                            ")"))
        (deliver (nth slots i) (screenshot! cdp)))
      (catch Throwable e
        (doseq [i mine]
          (deliver (nth slots i) e))))))

(defn- write-frames!
  "The screenshots into ffmpeg, in order, as they come."
  [in {:keys [slots written on-progress]}]
  (doseq [[i slot] (map-indexed vector slots)]
    (let [png @slot]
      (when (instance? Throwable png)
        (throw png))
      (.write in ^bytes png)
      (reset! written (inc i))
      (when on-progress
        (on-progress (inc i) (count slots))))))

(defn render!
  "Renders the design to `:out`, a path relative to the working directory,
  and returns {:out :frames :seconds}. The options are in the namespace;
  `:fps` is 30, `:dir` the working directory, `:chrome` is found when
  absent, `:ffmpeg` on the PATH, `:workers` the Chromes that screenshot
  side by side (`default-workers`), `:on-progress` a function of the
  frames done and the frames in all."
  [{:keys [frame design duration fps size head background dir audio out chrome ffmpeg workers on-progress]
    :or {fps 30 dir "."}}]
  (let [chrome (or chrome
                   (find-chrome)
                   (throw (ex-info "No Chrome: CHROME in the environment, or google-chrome or chromium on the PATH." {})))
        workers (max 1 (long (or workers (default-workers))))
        dir (fs/absolutize (fs/file dir))
        ;; H.264 wants even sizes.
        size (mapv #(* 2 (quot (long %) 2)) size)
        times (frame-times duration fps)
        workers (min workers (max 1 (count times)))
        page (fs/file dir ".fnmotion-render.html")
        out (fs/absolutize (fs/file out))
        started (System/nanoTime)]
    (spit (str page) (page-html {:size size :head head :background background}))
    (let [chromes (mapv (fn [_] (start-chrome! chrome size)) (range workers))]
      (try
        (let [cdps (mapv (fn [{:keys [port]}]
                           (prepare-page! (connect! (page-url port)) size page))
                         chromes)
              encoder (process/process (ffmpeg-args {:ffmpeg ffmpeg :fps fps :duration duration :audio audio :out out})
                                       {:dir (str dir) :in :stream :out :string :err :string})]
          (try
            (let [job {:frame frame
                       :design design
                       :times times
                       :slots (vec (repeatedly (count times) promise))
                       :written (atom 0)
                       :workers workers
                       :on-progress on-progress}
                  running (doall (map-indexed (fn [k cdp] (future (work! cdp job k))) cdps))]
              (with-open [in (:in encoder)]
                (write-frames! in job))
              (run! deref running))
            (let [{:keys [exit err]} @encoder]
              (when-not (zero? exit)
                (throw (ex-info (str "ffmpeg failed: " (str/trim (str err))) {:exit exit}))))
            {:out (str out)
             :frames (count times)
             :workers workers
             :seconds (/ (- (System/nanoTime) started) 1e9)}
            (catch Throwable e
              (fs/delete-if-exists out)
              (throw e))
            (finally
              (doseq [cdp cdps]
                (ws/close! (:socket cdp)))
              (when (.isAlive (:proc encoder))
                (process/destroy-tree encoder)))))
        (finally
          (doseq [{:keys [process profile]} chromes]
            (stop-chrome! process profile))
          (fs/delete-if-exists page))))))

(comment
  (render! {:frame (fn [{:keys [color]} t]
                     [:div {:style {:position "absolute" :inset 0 :background color
                                    :opacity (/ t 2)}}])
            :design {:color "#b45309"}
            :duration 2
            :size [320 180]
            :audio [{:src "tone.wav" :volume 0.5 :fade-out 0.5}]
            :out "/tmp/fnmotion.mp4"})
  )
