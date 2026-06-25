(ns smoke.video
  "Offline, frame-by-frame video renderer. Runs the smoke sim with NO window and
   pipes each rendered frame straight into ffmpeg as raw RGB24, muxing the audio
   track in the same pass => one self-contained MP4. This is the path for a LARGE,
   crisp output (e.g. 1080p, or a finer sim grid) that real-time playback can't
   sustain: the sim/render are parallelised (smoke.fluid/par-rows), but offline we
   don't care about wall-clock fps, so we can crank the resolution arbitrarily.

   Audio reactivity is driven by FRAME INDEX (frame/fps), not the wall clock
   (smoke.audio/modulate!), so the rendered file is sample-accurate in sync.

     (require '[smoke.video :as v])
     ;; 1080p audio-reactive clip from a WAV (whole track):
     (v/render! \"/tmp/out.mp4\" :audio \"/tmp/song.wav\" :preset :galaxy-slime
                :render [1920 1080])
     ;; first 20 s only, finer 512 grid:
     (v/render! \"/tmp/out.mp4\" :audio \"/tmp/song.wav\" :render [1920 1080]
                :grid 512 :seconds 20)
     ;; silent visual, square 1440:
     (v/render! \"/tmp/out.mp4\" :seconds 12 :render [1440 1440])

   Audio note: analysis uses javax.sound, which decodes only PCM WAV/AIFF — pass a
   WAV. (ffmpeg muxes whatever you pass, but the analyser needs WAV/AIFF.)
   Convert first if needed:  ffmpeg -i song.mp3 song.wav"
  (:require [smoke.scene :as scene]
            [smoke.fluid :as f]
            [smoke.audio :as audio])
  (:import [java.io OutputStream File]
           [java.lang ProcessBuilder ProcessBuilder$Redirect]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- pack-rgb24!
  "Pack an int-ARGB pixel buffer into a preallocated rgb24 byte[] (parallel rows)."
  [^ints px ^bytes out ^long w ^long h]
  (f/par-rows
   h
   (fn [^long y]
     (let [row (* y w) brow (* 3 row)]
       (dotimes [x w]
         (let [v (aget px (+ row x)) o (+ brow (* 3 x))]
           (aset out o          (unchecked-byte (bit-and (bit-shift-right v 16) 0xFF)))
           (aset out (+ o 1)    (unchecked-byte (bit-and (bit-shift-right v 8) 0xFF)))
           (aset out (+ o 2)    (unchecked-byte (bit-and v 0xFF)))))))))

(defn- ffmpeg-cmd
  "Argv for ffmpeg reading raw rgb24 frames from stdin, optionally muxing `audio`."
  [out-path w h fps audio crf start-secs]
  (-> ["ffmpeg" "-y" "-hide_banner" "-loglevel" "warning"
       ;; video from stdin (raw frames)
       "-f" "rawvideo" "-pixel_format" "rgb24"
       "-video_size" (str w "x" h) "-framerate" (str fps) "-i" "pipe:0"]
      ;; audio input, optionally seeked so it lines up with :start-secs
      (cond-> (and audio (pos? (double start-secs))) (into ["-ss" (str start-secs)]))
      (cond-> audio (into ["-i" audio]))
      (into ["-c:v" "libx264" "-pix_fmt" "yuv420p" "-preset" "medium" "-crf" (str crf)])
      (cond-> audio (into ["-c:a" "aac" "-b:a" "192k" "-shortest"]))
      (conj out-path)))

(defn render!
  "Render an offline smoke video to `out-path` (MP4). Options:
     :audio    WAV/AIFF path — drives audio-reactive modulation AND is muxed in.
     :fps      frames/sec (default 60). Must drive the analysis hop rate too.
     :seconds  duration override; defaults to (audio length - start-secs), else 10 s.
     :start-secs  start offset into the track (seeks audio + modulation); default 0.
                  Handy to preview a beaty section without rendering from the top.
     :render   [w h] render resolution (default the params/square default).
     :grid     sim grid size n (default scene/N); higher = finer smoke (costlier).
     :preset   a scene preset keyword (see scene/presets) merged into params.
     :params   extra param override map (applied last; wins over preset/render/grid).
     :crf      libx264 quality, lower = better/bigger (default 18).
     :warmup   sim steps to run before recording, so frame 0 isn't empty (default 90).
   Returns {:out path :frames n :render [w h] :fps fps :seconds dur}."
  [out-path & {:keys [audio fps seconds start-secs render grid preset params crf warmup]
               :or   {fps 60 crf 18 warmup 90 start-secs 0}}]
  (let [fps (long fps) start (double start-secs)
        p   (cond-> scene/default-params
              preset (merge (or (scene/preset-params preset)
                                (throw (ex-info "unknown preset" {:preset preset}))))
              render (assoc :render-w (long (first render)) :render-h (long (second render)))
              grid   (assoc :grid-n (long grid))
              params (merge params))
        analysis (when audio (audio/analyze audio :bands (audio/band-count p) :fps fps))
        dur (double (or seconds (when analysis (- (:dur-secs analysis) start)) 10))
        nframes (long (Math/round (* (double fps) dur)))
        w (scene/render-w p) h (scene/render-h p)
        ff (ffmpeg-cmd out-path w h fps audio crf start)
        log (File/createTempFile "smoke-ffmpeg" ".log")
        proc (.start (doto (ProcessBuilder. ^java.util.List ff)
                       (.redirectOutput ProcessBuilder$Redirect/DISCARD)
                       (.redirectError (ProcessBuilder$Redirect/to log))))
        ^OutputStream os (.getOutputStream proc)
        px  (int-array (* w h))
        buf (byte-array (* 3 w h))
        t0  (System/nanoTime)]
    (println (format "rendering %d frames @ %dx%d %dfps (grid %d) -> %s"
                     nframes w h fps (scene/grid-n p) out-path))
    (when analysis (audio/offline-init! analysis))
    (try
      (loop [fl (scene/new-fluid p) i (- (long warmup))]
        (cond
          ;; record warmup steps first (don't count toward output frames)
          (neg? i)
          (recur (scene/advance (doto fl (scene/seed-sources! p)) p) (inc i))

          (>= i nframes) nil

          :else
          (do
            (when analysis (audio/modulate! analysis (+ start (/ (double i) fps)) p))
            (scene/seed-sources! fl p)
            (let [fl (scene/advance fl p)]
              (scene/render-pixels! fl p px)
              (pack-rgb24! px buf w h)
              (.write os buf)
              (when (zero? (rem i 60))
                (println (format "  frame %d/%d (%.1fs)  %.1f fps render"
                                 i nframes (/ (double i) fps)
                                 (if (pos? i) (/ (double i) (/ (- (System/nanoTime) t0) 1.0e9)) 0.0))))
              (recur fl (inc i))))))
      (finally
        (.close os)
        ;; don't leak this render's last audio modulation into a later live session
        (when analysis
          (reset! scene/audio-keep nil) (reset! scene/audio-dt nil)
          (reset! scene/audio-emit nil) (reset! scene/audio-gains nil))))
    (.waitFor proc)
    (let [code (.exitValue proc)]
      (when-not (zero? code)
        (println "ffmpeg FAILED, log:" (.getPath log))
        (println (slurp log))))
    {:out out-path :frames nframes :render [w h] :fps fps :seconds dur
     :exit (.exitValue proc) :ffmpeg-log (.getPath log)}))

(comment
  (require '[smoke.video :as v] :reload)
  ;; whole-track 1080p hero look:
  (v/render! "/tmp/smoke.mp4" :audio "/tmp/song.wav" :preset :galaxy-slime :render [1920 1080])
  ;; quick silent 5 s test:
  (v/render! "/tmp/test.mp4" :seconds 5 :render [1280 720]))
