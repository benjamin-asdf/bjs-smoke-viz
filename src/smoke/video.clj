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
            [smoke.audio :as audio]
            [clojure.string]
            [clojure.pprint]
            [clojure.edn]
            [clojure.java.shell :as sh])
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
  "Argv for ffmpeg reading raw rgb24 frames from stdin, optionally muxing `audio`.
   When `tail?` (a silent visual tail past the audio), DON'T pass -shortest, so the
   longer video stream sets the duration and the last seconds are simply silent.
   When `pad` = [W H], centre the rendered frame on a black W×H canvas WITHOUT
   scaling (letter/pillar-box), so a square render keeps its shape at 16:9."
  [out-path w h fps audio crf start-secs tail? pad sharpen]
  (let [filters (cond-> []
                  ;; unsharp mask => crisp edges even from a coarse sim grid
                  (and sharpen (pos? (double sharpen)))
                  (conj (format "unsharp=5:5:%.3f:5:5:0.0" (double sharpen)))
                  ;; pad onto black at NATIVE size (no scale): centre WxH in the target
                  pad
                  (conj (format "pad=%d:%d:(ow-iw)/2:(oh-ih)/2:black"
                                (long (first pad)) (long (second pad)))))]
    (-> ["ffmpeg" "-y" "-hide_banner" "-loglevel" "warning"
         ;; video from stdin (raw frames)
         "-f" "rawvideo" "-pixel_format" "rgb24"
         "-video_size" (str w "x" h) "-framerate" (str fps) "-i" "pipe:0"]
        ;; audio input, optionally seeked so it lines up with :start-secs
        (cond-> (and audio (pos? (double start-secs))) (into ["-ss" (str start-secs)]))
        (cond-> audio (into ["-i" audio]))
        (cond-> (seq filters) (into ["-vf" (clojure.string/join "," filters)]))
        (into ["-c:v" "libx264" "-pix_fmt" "yuv420p" "-preset" "medium" "-crf" (str crf)])
        (cond-> audio (into ["-c:a" "aac" "-b:a" "192k"]))
        (cond-> (and audio (not tail?)) (into ["-shortest"]))
        (conj out-path))))

;; ---------------------------------------------------------------------------
;; :arrange — auto-switch the LOOK between the track's quiet and loud sections
;; (haze in the verses, cyber in the drop). We detect sections offline from the
;; loudness envelope, then per frame merge the section's param override onto the
;; base preset, CROSSFADING numeric params across the boundary so it glides.
;; Keep the :theme constant across overrides — a mid-render theme switch would
;; need the agents/flock rebuilt; :arrange only morphs the numeric character.

(defn- lerp-params
  "Blend two effective param maps: shared NUMERIC keys interpolate by `t` (0->a,
   1->b); anything else snaps to a below 0.5, b above."
  [a b ^double t]
  (reduce (fn [m k]
            (let [va (get a k) vb (get b k)]
              (assoc m k (if (and (number? va) (number? vb))
                           (+ (double va) (* t (- (double vb) (double va))))
                           (if (< t 0.5) va vb)))))
          {} (distinct (concat (keys a) (keys b)))))

(defn- section-idx ^long [sections ^double secs]
  (or (first (keep-indexed (fn [i s] (when (and (>= secs (double (:start s)))
                                                (< secs (double (:end s)))) i)) sections))
      (dec (count sections))))

(defn- arrange-p
  "Effective params at `secs`: the base preset merged with the current section's
   override (:loud or :quiet from `arrange`), crossfaded to the neighbour across a
   `:fade`-second window centred on each boundary (0.5 blend exactly at the edge)."
  [base sections arrange ^double secs]
  (let [fade (double (:fade arrange 1.0)) half (* 0.5 fade)
        pick (fn [sec] (merge base (get arrange (if (:loud? sec) :loud :quiet))))
        idx  (section-idx sections secs)
        cur  (nth sections idx)
        ds   (- secs (double (:start cur))) de (- (double (:end cur)) secs)]
    (cond
      (and (pos? idx) (< ds half))
      (lerp-params (pick (nth sections (dec idx))) (pick cur) (/ (+ ds half) fade))
      (and (< (inc idx) (count sections)) (< de half))
      (lerp-params (pick cur) (pick (nth sections (inc idx))) (/ (- half de) fade))
      :else (pick cur))))

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
     :tail-secs  extra seconds rendered AFTER the audio with modulation OFF — the
                 visuals keep running but stop reacting (audio atoms cleared), so you
                 see the look settle 'without audio'. The video runs that much longer
                 than the (silent-tailed) audio. Default 0.
     :pad      [W H] — centre the rendered frame on a black W×H canvas at NATIVE
                 size (no scaling). Use with a square :render to put it on a 16:9
                 YouTube frame with black side-bars instead of cropping/stretching.
     :arrange  {:loud <param-map> :quiet <param-map> :fade 1.0 :thresh 1.0 :min-secs 4.0}
                 — auto-switch the LOOK between the track's loud and quiet sections
                 (needs :audio). The sections are detected offline (audio/sections);
                 each song frame merges its section's override onto the base preset,
                 crossfading numeric params across :fade seconds at each boundary.
                 Keep :theme the same in both overrides — this morphs the numeric
                 character (flow/keep/saturation/colour-cycle…), not the structure.
   Returns {:out path :frames n :render [w h] :fps fps :seconds dur}."
  [out-path & {:keys [audio fps seconds start-secs render grid preset params crf warmup tail-secs pad sharpen arrange]
               :or   {fps 60 crf 18 warmup 90 start-secs 0 tail-secs 0}}]
  (let [fps (long fps) start (double start-secs)
        p   (cond-> scene/default-params
              preset (merge (or (scene/preset-params preset)
                                (throw (ex-info "unknown preset" {:preset preset}))))
              render (assoc :render-w (long (first render)) :render-h (long (second render)))
              grid   (assoc :grid-n (long grid))
              params (merge params))
        analysis (when audio (audio/analyze audio :bands (audio/band-count p) :fps fps))
        ;; :arrange => precompute loud/quiet sections; each song frame morphs the
        ;; base params toward the current section's override (crossfaded at edges)
        sections (when (and arrange analysis)
                   (audio/sections analysis :thresh (double (:thresh arrange 1.0))
                                   :min-secs (double (:min-secs arrange 4.0))))
        dur (double (or seconds (when analysis (- (:dur-secs analysis) start)) 10))
        nframes (long (Math/round (* (double fps) dur)))
        tail-frames (long (Math/round (* (double fps) (double tail-secs))))
        total   (+ nframes tail-frames)
        w (scene/render-w p) h (scene/render-h p)
        ff (ffmpeg-cmd out-path w h fps audio crf start (pos? tail-frames) pad sharpen)
        log (File/createTempFile "smoke-ffmpeg" ".log")
        proc (.start (doto (ProcessBuilder. ^java.util.List ff)
                       (.redirectOutput ProcessBuilder$Redirect/DISCARD)
                       (.redirectError (ProcessBuilder$Redirect/to log))))
        ^OutputStream os (.getOutputStream proc)
        px  (int-array (* w h))
        buf (byte-array (* 3 w h))
        t0  (System/nanoTime)]
    (println (format "rendering %d frames (%d song + %d silent tail) @ %dx%d %dfps (grid %d) -> %s"
                     total nframes tail-frames w h fps (scene/grid-n p) out-path))
    (when sections
      (println (format "  arrange: %d sections" (count sections)))
      (doseq [s sections]
        (println (format "    %5.1f-%5.1fs %-5s energy %.3f"
                         (double (:start s)) (double (:end s))
                         (if (:loud? s) "LOUD" "quiet") (double (:energy s))))))
    (when analysis (audio/offline-init! analysis p))
    (try
      (loop [fl (scene/new-fluid p) i (- (long warmup))]
        (cond
          ;; record warmup steps first (don't count toward output frames)
          (neg? i)
          (recur (scene/advance (doto fl (scene/seed-sources! p)) p) (inc i))

          (>= i total) nil

          :else
          (do
            ;; song portion: drive modulation. At the tail boundary, clear all audio
            ;; atoms ONCE so the tail runs with NO reactivity (the 'without audio' look).
            (cond
              ;; :audio-lead-secs looks AHEAD so deposits fire slightly early and the
              ;; smoke's bloom peaks ON the beat (compensates the keep/advection lag)
              (< i nframes) (when analysis
                              (audio/modulate! analysis (+ start (/ (double i) fps)
                                                           (double (:audio-lead-secs p 0.0))) p))
              (= i nframes) (do (reset! scene/audio-keep nil) (reset! scene/audio-dt nil)
                                (reset! scene/audio-emit nil) (reset! scene/audio-gains nil)
                                (reset! scene/audio-wind nil) (reset! scene/audio-pulse nil)
                                (reset! scene/audio-puffs [])))
            ;; per-frame params: base, or the section-morphed params when :arrange is on
            (let [pf (if (and sections (< i nframes))
                       (arrange-p p sections arrange (+ start (/ (double i) fps)))
                       p)]
              (scene/seed-sources! fl pf)
              (let [fl (scene/advance fl pf)]
                (scene/render-pixels! fl pf px)
                (pack-rgb24! px buf w h)
                (.write os buf)
                (when (zero? (rem i 60))
                  (println (format "  frame %d/%d (%.1fs)%s  %.1f fps render"
                                   i total (/ (double i) fps)
                                   (if (>= i nframes) " [tail]" "")
                                   (if (pos? i) (/ (double i) (/ (- (System/nanoTime) t0) 1.0e9)) 0.0))))
                (recur fl (inc i)))))))
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
    {:out out-path :frames total :song-frames nframes :tail-frames tail-frames
     :render [w h] :fps fps :seconds (/ (double total) fps)
     :exit (.exitValue proc) :ffmpeg-log (.getPath log)}))

;; ---------------------------------------------------------------------------
;; Instagram/TikTok Reels: vertical 9:16 convenience wrapper.

(def ^:const REEL-W 1080)   ; Instagram Reels / TikTok native portrait size
(def ^:const REEL-H 1920)

(defn- ping-pong!
  "Post-process `in` mp4 into a forward+reverse (boomerang) clip at `out` — a
   guaranteed-seamless loop (video only; audio is dropped). Re-encodes."
  [in out crf]
  (let [log (File/createTempFile "smoke-loop" ".log")
        ff  ["ffmpeg" "-y" "-hide_banner" "-loglevel" "warning" "-i" in
             "-filter_complex" "[0:v]split[a][b];[b]reverse[r];[a][r]concat=n=2:v=1:a=0[v]"
             "-map" "[v]" "-c:v" "libx264" "-pix_fmt" "yuv420p" "-preset" "medium"
             "-crf" (str crf) out]
        proc (.start (doto (ProcessBuilder. ^java.util.List ff)
                       (.redirectOutput ProcessBuilder$Redirect/DISCARD)
                       (.redirectError (ProcessBuilder$Redirect/to log))))]
    (.waitFor proc)
    {:exit (.exitValue proc) :ffmpeg-log (.getPath log)}))

(defn- git-info
  "Provenance for the manifest: HEAD commit, `git describe`, and whether the tree
   is dirty (the reel was rendered from uncommitted changes). nil if not a repo."
  []
  (try
    (let [run (fn [& args]
                (let [r (apply sh/sh "git" args)]
                  (when (zero? (long (:exit r))) (clojure.string/trim (:out r)))))]
      {:commit   (run "rev-parse" "HEAD")
       :describe (run "describe" "--always" "--dirty" "--tags")
       :dirty?   (not (clojure.string/blank? (run "status" "--porcelain")))})
    (catch Exception _ nil)))

(defn- write-sidecar!
  "Write a self-documenting <out>.edn next to the reel so every clip records HOW it
   was made and leaves room to record HOW IT PERFORMED:
     :id      stable id (= output basename) — link an Insta post back to this
     :params  the effective reel! options used
     :repro   the exact (smoke.video/reel! ...) form to regenerate it
     :git     commit / describe / dirty? at render time
     :rendered-at  ISO-8601 timestamp
     :result  frames / resolution / fps / duration
     :insta   EMPTY metrics slot — fill in posted-url + views/likes after posting,
              then reach can be joined back to :params to tune what works."
  [out-path call-opts result]
  (let [sidecar (str out-path ".edn")
        id      (clojure.string/replace (.getName (File. ^String out-path)) #"\.[^.]+$" "")
        data    {:id          id
                 :out         out-path
                 :rendered-at (str (java.time.Instant/now))
                 :git         (git-info)
                 :params      call-opts
                 :repro       (pr-str (concat (list 'smoke.video/reel! out-path)
                                              (mapcat identity call-opts)))
                 :result      (select-keys result [:frames :song-frames :tail-frames
                                                   :render :fps :seconds :boomerang :exit])
                 ;; fill in after posting → lets us correlate reach with :params:
                 :insta       {:posted-url nil :posted-at nil
                               :views nil :likes nil :comments nil :shares nil :saves nil}}]
    (spit sidecar (with-out-str (clojure.pprint/pprint data)))
    sidecar))

(defn reel!
  "Render a vertical 9:16 clip sized for Instagram Reels / TikTok (1080x1920).
   Thin wrapper over `render!`: forces portrait :render and applies reel-friendly
   defaults, but every `render!` option can still be overridden via kwargs.
   Defaults: :fps 30, :seconds 12, :preset :galaxy-slime. The square sim grid is
   cover-fit into the portrait frame (centre crop), so any look works.
   Extra options:
     :boomerang  when true AND there is no :audio, append a reversed copy so the
                 clip ping-pongs into a seamless loop (popular silent-reel look).
                 Doubles the final duration. Ignored when :audio is set.
     :manifest?  write a <out>.edn provenance sidecar (default true) — params,
                 repro command, git commit, timestamp, and an Insta metrics slot.
   Returns the render! result map (with :out = out-path)."
  [out-path & {:as opts}]
  (let [audio     (:audio opts)
        manifest? (:manifest? opts true)
        crf       (long (:crf opts 18))
        ;; effective, reproducible options (portrait size forced last)
        call-opts (merge {:fps 30 :seconds 12 :preset :galaxy-slime}
                         (dissoc opts :manifest?)
                         {:render [REEL-W REEL-H]})
        boomerang (:boomerang call-opts)
        render-opts (dissoc call-opts :boomerang)
        result    (if (and boomerang (not audio))
                    (let [tmp (.getPath (File/createTempFile "smoke-reel" ".mp4"))
                          r   (apply render! tmp (mapcat identity render-opts))]
                      (ping-pong! tmp out-path crf)
                      (assoc r :out out-path :boomerang true
                             :seconds (* 2.0 (double (:seconds r)))))
                    (do (when (and boomerang audio)
                          (println "reel!: :boomerang ignored with :audio (reversed audio sounds wrong)"))
                        (apply render! out-path (mapcat identity render-opts))))]
    (when manifest? (write-sidecar! out-path call-opts result))
    result))

;; ---------------------------------------------------------------------------
;; Quick GIF previews: render a short, low-res, no-audio clip of each preset and
;; a labelled montage, so you pick a look at a glance instead of watching full
;; renders. Fast by design: small grid + short + low fps. Needs ffmpeg (+ mpv
;; only for playback elsewhere) and ImageMagick `montage` for the contact sheet.

(defn- sh-ok!
  "Run argv, printing + throwing with the captured stderr on a non-zero exit."
  [& argv]
  (let [r (apply sh/sh argv)]
    (when-not (zero? (long (:exit r)))
      (throw (ex-info (str (first argv) " failed") {:argv argv :err (:err r)})))
    r))

(defn preview-gif!
  "Render a short, low-res, NO-audio looping GIF of one `preset` to `out` (.gif),
   for quick look-picking. Renders a tiny square mp4 then palettegen->gif. Fast:
   defaults to a 192-cell grid, 256px, 2s @ 20fps. Audio-reactive presets show
   only their resting motion here (no beat) — enough to judge the look. Returns
   `out`. Options match a subset of render! plus :size (gif px) and :params."
  [preset out & {:keys [secs size fps grid warmup params]
                 :or {secs 2.0 size 256 fps 20 grid 192 warmup 120}}]
  (let [tmp (.getPath (File/createTempFile "smoke-preview" ".mp4"))]
    (apply render! tmp (concat [:preset preset :seconds secs :fps fps :grid grid
                                :warmup warmup :render [size size] :crf 20]
                               (when params [:params params])))
    ;; single-pass gif with a per-clip optimal palette (split -> palettegen/use)
    (sh-ok! "ffmpeg" "-y" "-hide_banner" "-loglevel" "error" "-i" tmp
            "-vf" (format "fps=%d,scale=%d:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse"
                          (long fps) (long size))
            "-loop" "0" out)
    (.delete (File. tmp))
    out))

(defn contact-sheet!
  "Render a quick preview GIF of each preset in `presets` (default: every preset
   in scene/presets) into `dir`, then montage a labelled grid PNG (first frame of
   each, captioned with the preset name) for at-a-glance picking. Watch the moving
   look by opening the individual <preset>.gif files. Returns
   {:gifs [[preset path]...] :sheet <png>}. Extra opts pass through to preview-gif!."
  [& {:keys [presets dir cols] :or {dir "media/previews" cols 4} :as opts}]
  (let [presets (or presets (mapv first scene/presets))
        pgopts  (mapcat identity (dissoc opts :presets :dir :cols))]
    (.mkdirs (File. ^String dir))
    (let [gifs (mapv (fn [preset]
                       (let [g (str dir "/" (name preset) ".gif")]
                         (println "preview:" preset "->" g)
                         (apply preview-gif! preset g pgopts)
                         [preset g]))
                     presets)
          ;; grab frame 0 of each gif as a labelled PNG tile, then montage
          tiles (mapv (fn [[preset g]]
                        (let [png (str dir "/." (name preset) ".tile.png")]
                          (sh-ok! "convert" (str g "[0]") png)
                          [preset png]))
                      gifs)
          sheet (str dir "/CONTACT-SHEET.png")]
      (apply sh-ok! (concat ["montage"]
                            (mapcat (fn [[preset png]] ["-label" (name preset) png]) tiles)
                            ["-tile" (str cols "x") "-geometry" "+6+6"
                             "-background" "black" "-fill" "white" "-pointsize" "16" sheet]))
      (doseq [[_ png] tiles] (.delete (File. ^String png)))
      (println "contact sheet:" sheet)
      {:gifs gifs :sheet sheet})))

;; ---------------------------------------------------------------------------
;; Manifest + reach analysis: aggregate the per-reel sidecars, and once the
;; :insta metrics are filled in (after posting), rank what gets reach so we tune.

(defn scan-reels
  "Read every <reel>.edn sidecar in `dir` into a vector of metadata maps."
  [dir]
  (->> (seq (.listFiles (File. ^String dir)))
       (map (fn [^File f] (.getName f)))
       (filter #(clojure.string/ends-with? % ".edn"))
       sort
       (keep #(try (clojure.edn/read-string (slurp (str dir "/" %))) (catch Exception _ nil)))
       vec))

(defn manifest-md!
  "Aggregate every sidecar in `dir` into `dir`/MANIFEST.md — one row per reel with
   track/preset/params, git commit, timestamp and reach columns (blank until the
   `:insta` metrics are filled into the sidecars after posting)."
  [dir]
  (let [rows (scan-reels dir)
        cell (fn [v] (if (nil? v) "" (str v)))
        ts   (fn [m] (let [s (cell (:rendered-at m))] (subs s 0 (min 19 (count s)))))
        line (fn [m]
               (let [p (:params m) i (:insta m)]
                 (str "| " (:id m) " | " (cell (:preset p)) " | " (cell (:audio p))
                      " | " (cell (:seconds p)) "s | " (cell (:describe (:git m)))
                      " | " (ts m) " | " (cell (:views i)) " | " (cell (:likes i))
                      " | " (cell (:posted-url i)) " |")))
        md   (str "# Reels manifest\n\n"
                  "Auto-generated from the per-reel `.edn` sidecars via "
                  "`(smoke.video/manifest-md! \"" dir "\")`.\n"
                  "Reach columns stay blank until you fill `:insta` in each sidecar "
                  "after posting; then `(smoke.video/reach-report \"" dir "\")`.\n\n"
                  "| id | preset | audio | len | commit | rendered (UTC) | views | likes | url |\n"
                  "|----|--------|-------|-----|--------|----------------|-------|-------|-----|\n"
                  (clojure.string/join "\n" (map line rows)) "\n")]
    (spit (str dir "/MANIFEST.md") md)
    {:reels (count rows) :manifest (str dir "/MANIFEST.md")}))

(defn reach-report
  "Once `:insta`/`:views` are filled in the sidecars, rank reels and average reach
   by preset and by track => what to make more of. Reels without :views are
   skipped. Returns {:by-reel [[id views]...] :by-preset ... :by-track ...}."
  [dir]
  (let [rows  (filter #(number? (:views (:insta %))) (scan-reels dir))
        views (fn [m] (double (:views (:insta m))))
        track (fn [m] (first (clojure.string/split (str (:id m)) #"-" 2)))
        avg   (fn [ms] (when (seq ms) (/ (reduce + (map views ms)) (count ms))))
        grp   (fn [kf] (->> (group-by kf rows)
                            (map (fn [[k ms]] [k {:n (count ms) :avg-views (avg ms)}]))
                            (sort-by (comp - double :avg-views second))
                            vec))]
    {:by-reel   (->> rows (sort-by (comp - views)) (mapv #(vector (:id %) (long (views %)))))
     :by-preset (grp #(:preset (:params %)))
     :by-track  (grp track)}))

(comment
  (require '[smoke.video :as v] :reload)
  ;; whole-track 1080p hero look:
  (v/render! "/tmp/smoke.mp4" :audio "/tmp/song.wav" :preset :galaxy-slime :render [1920 1080])
  ;; quick silent 5 s test:
  (v/render! "/tmp/test.mp4" :seconds 5 :render [1280 720])
  ;; vertical Insta reel, audio-reactive, 12 s of a track:
  (v/reel! "reels/brejcha-slime.mp4" :audio "music/brejcha.wav" :preset :galaxy-slime)
  ;; silent seamless boomerang loop:
  (v/reel! "reels/loop.mp4" :preset :swarm :seconds 6 :boomerang true)

  ;; ── :arrange — verse haze -> drop cyber (auto-switch look by loudness) ──
  ;; inspect the detected sections first (tune :thresh / :min-secs):
  (audio/sections (audio/analyze "media/music/brejcha.wav" :bands 10 :fps 60) :min-secs 6.0)
  (v/render! "/tmp/arranged.mp4" :audio "media/music/brejcha.wav" :preset :boid-slime-reactive
             :arrange {:quiet {:saturation 2.2 :flock-flow 0.0 :keep 0.97 :audio-color-cycle 0}
                       :loud  {:saturation 4.5 :flock-flow 8.0 :keep 0.90 :audio-color-cycle 8}
                       :fade 2.0 :min-secs 8.0})

  ;; ── vocal-ONSET bloom — a flash on each sung attack (needs vocals) ──
  (v/reel! "reels/cecilia-voice.mp4" :audio "media/music/ceciliaasoro.wav" :preset :galaxy-slime
           :params {:voice-onset? true :voice-onset-thresh 0.05 :voice-decay 0.82 :voice-bloom 0.9
                    :pulse-band-lo 0.15 :pulse-band-hi 0.6})

  ;; ── quick-GIF previews — pick a look at a glance, then watch the .gif ──
  (v/preview-gif! :cyber-flock "media/previews/cyber-flock.gif")
  (v/contact-sheet!)                                   ; every preset -> media/previews/CONTACT-SHEET.png
  (v/contact-sheet! :presets [:galaxy-slime :boid-rivers :cyber-flock :frost] :cols 2))
