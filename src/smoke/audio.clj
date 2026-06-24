(ns smoke.audio
  "Audio-reactive layer. Pre-analyse a WAV/AIFF file into a per-hop timeline of
   frequency-band gains (reusing the JTransforms FFT), play it via an EXTERNAL
   player (mpv/ffplay/paplay) and, once per render frame, modulate the smoke's
   per-colour `keep` (and dt on beats) from the live band energy. Playback is
   external + wall-clock-synced because javax.sound's Clip blocks/contends badly
   on PipeWire/ALSA; we use javax.sound only to decode for analysis.

   Bands map onto the ACTIVE PALETTE: band i drives palette colour i. Since the
   density field is plain R/G/B (no per-palette-colour separation), each band's
   gain is spread across the three keep factors weighted by its palette colour's
   RGB composition — so when a band is loud, smoke carrying that colour lingers.

     (require '[smoke.audio :as a])
     (a/play-with-sim! \"/path/song.wav\")  ; start sketch + audio together
     (a/start! \"/path/song.wav\" :amp 0.05) ; sketch already running
     (a/stop!)                               ; stop audio + clear modulation

   Java only decodes PCM WAV/AIFF. Convert other formats first, e.g.
     ffmpeg -i song.mp3 song.wav"
  (:require [smoke.core :as core]
            [smoke.scene :as scene])
  (:import [javax.sound.sampled AudioSystem AudioFormat AudioFormat$Encoding]
           [org.jtransforms.fft DoubleFFT_1D]
           [java.io File]
           [java.lang ProcessBuilder$Redirect]))

(set! *warn-on-reflection* true)

;; ---- decode ----------------------------------------------------------------
(defn- read-pcm
  "Decode `path` (WAV/AIFF) to mono doubles in [-1,1] plus the sample rate."
  [path]
  (with-open [in0 (AudioSystem/getAudioInputStream (File. ^String path))]
    (let [base   (.getFormat in0)
          sr     (.getSampleRate base)
          ch     (.getChannels base)
          target (AudioFormat. AudioFormat$Encoding/PCM_SIGNED sr 16 ch (* 2 ch)
                               sr false)            ; 16-bit signed little-endian
          in     (AudioSystem/getAudioInputStream target in0)
          ^bytes raw (.readAllBytes in)
          frames (quot (alength raw) (* 2 ch))
          out    (double-array frames)]
      (dotimes [i frames]
        (let [acc (loop [c 0 s 0]
                    (if (< c ch)
                      (let [b   (* 2 (+ (* i ch) c))
                            lo  (bit-and (aget raw b) 0xff)
                            hi  (aget raw (inc b))          ; signed high byte
                            val (bit-or (bit-shift-left hi 8) lo)]
                        (recur (inc c) (+ s val)))
                      s))]
          (aset out i (/ (double acc) (* (double ch) 32768.0)))))
      {:samples out :rate (double sr)})))

;; ---- analysis --------------------------------------------------------------
(defn- band-edges
  "Log-spaced FFT-bin edges for `bands` buckets from ~40 Hz to Nyquist."
  [bands fft-n rate]
  (let [lo 40.0 hi (/ (double rate) 2.0) bins-per-hz (/ (double fft-n) (double rate))]
    (mapv (fn [b]
            (long (* bins-per-hz (* lo (Math/pow (/ hi lo) (/ (double b) (double bands)))))))
          (range (inc bands)))))

(defn analyze
  "Pre-analyse `path` into {:hops [double[bands] ...] :hop-secs :bands :dur-secs}.
   Each band gain is normalised to 0..1 over the whole track. `fps` should match
   the render frame rate; `fft-n` is the (power-of-two) window size."
  [path & {:keys [bands fps fft-n] :or {bands 3 fps 60 fft-n 2048}}]
  (let [{:keys [^doubles samples ^double rate]} (read-pcm path)
        nsamp (alength samples)
        hop   (max 1 (long (/ rate (double fps))))
        nhops (max 1 (long (Math/ceil (/ (double nsamp) (double hop)))))
        fft   (DoubleFFT_1D. (long fft-n))
        edges (band-edges bands fft-n rate)
        half  (quot (long fft-n) 2)
        hann  (double-array fft-n)
        buf   (double-array fft-n)
        raw   (object-array nhops)]
    (dotimes [i fft-n]
      (aset hann i (* 0.5 (- 1.0 (Math/cos (/ (* 2.0 Math/PI i) (dec (long fft-n))))))))
    (dotimes [h nhops]
      (let [start (* (long h) hop)]
        (dotimes [i fft-n]
          (let [s (+ start i)]
            (aset buf i (if (< s nsamp) (* (aget samples s) (aget hann i)) 0.0))))
        (.realForward fft buf)
        (let [g (double-array bands)]
          (dotimes [b bands]
            (let [b0 (max 1 (long (nth edges b)))
                  b1 (min half (long (nth edges (inc b))))]
              (aset g b (double (loop [k b0 acc 0.0]
                                  (if (< k b1)
                                    (let [re (aget buf (* 2 k)) im (aget buf (inc (* 2 k)))]
                                      (recur (inc k) (+ acc (Math/sqrt (+ (* re re) (* im im))))))
                                    (/ acc (double (max 1 (- b1 b0))))))))))
          (aset raw h g))))
    ;; normalise each band over the track by its peak; onset flux = summed
    ;; positive band-energy increase between hops (beat/onset detector)
    (let [maxes (double-array bands)
          flux  (double-array nhops)]
      (dotimes [h nhops]
        (let [^doubles g (aget raw h)]
          (dotimes [b bands] (when (> (aget g b) (aget maxes b)) (aset maxes b (aget g b))))
          (when (pos? h)
            (let [^doubles gp (aget raw (dec h))]
              (aset flux h (double (reduce (fn [a b] (+ (double a) (max 0.0 (- (aget g b) (aget gp b)))))
                                           0.0 (range bands))))))))
      ;; normalise flux by the 95th percentile (not the max) so typical beats reach
      ;; ~1.0 instead of being squashed by rare huge transients
      (let [sorted (double-array flux)
            _      (java.util.Arrays/sort sorted)
            p95    (aget sorted (long (* 0.95 (dec nhops))))
            norm   (if (pos? p95) p95 1.0)]
        (dotimes [h nhops] (aset flux h (min 1.0 (/ (aget flux h) norm)))))
      {:hops     (mapv (fn [^doubles g]
                         (double-array (map (fn [b] (let [m (aget maxes b)]
                                                      (if (pos? m) (/ (aget g b) m) 0.0)))
                                            (range bands))))
                       raw)
       :flux     flux
       :hop-secs (/ 1.0 (double fps))
       :bands    bands
       :rate     rate
       :dur-secs (/ (double nsamp) rate)})))

;; ---- per-frame modulation --------------------------------------------------
(defn- channel-keep
  "Per-channel keep [kr kg kb] from band `gains` and the active `palette`.
   Each band's gain is weighted by its palette colour's RGB content, so a loud
   band raises the keep of the channels that make up its colour. `contrib` maps
   from a silence floor (base - floor, smoke fades to near-nothing) up to a loud
   ceiling (base + amp, denser/more persistent)."
  [^doubles gains palette base amp floor]
  (let [pal  (vec palette)
        n    (min (count pal) (alength gains))
        lo   (- (double base) (double floor))
        span (+ (double floor) (double amp))]
    (mapv (fn [ch]
            (let [num (reduce (fn [a i] (+ (double a) (* (aget gains i) (double (nth (nth pal i) ch))))) 0.0 (range n))
                  den (reduce (fn [a i] (+ (double a) (double (nth (nth pal i) ch)))) 0.0 (range n))
                  contrib (if (pos? den) (/ num den) 0.0)]
              (-> (+ lo (* span contrib)) (max 0.0) (min 0.999))))
          [0 1 2])))

(defn- gains-at
  "Band gains at playback time `secs`, or nil past the end."
  [{:keys [hops hop-secs]} secs]
  (let [idx (long (/ (double secs) (double hop-secs)))]
    (when (< idx (count hops)) (nth hops idx))))

(defn- flux-at
  "Onset/beat strength (0..1) at playback time `secs`, or 0 past the end."
  [{:keys [^doubles flux hop-secs]} secs]
  (let [idx (long (/ (double secs) (double hop-secs)))]
    (if (and flux (< idx (alength flux))) (aget flux idx) 0.0)))

;; ---- player ----------------------------------------------------------------
;; Audio is played by an EXTERNAL process, not javax.sound's Clip: on PipeWire/
;; ALSA backends Clip.open/close block for many seconds and contend with the
;; render thread (massive frame-rate drop). An external player + wall-clock sync
;; sidesteps all of that; we keep javax.sound only to DECODE for analysis.
(defonce ^:private state (atom nil))  ; {:proc Process :analysis {..} :t0 <nanoTime>}
(defonce ^:private kick   (atom 0.0)) ; decaying beat-kick envelope (0..1)
(defonce ^:private beat-n (atom 0))   ; count of detected beats (for every-Nth accent)
(defonce ^:private armed  (atom false)) ; true while inside a beat (debounces the onset trigger)

(def ^:private KICK-DECAY 0.80)       ; per-frame decay => ~150ms tail on each onset
(def ^:private KICK-ATTACK 0.25)      ; per-frame rise toward an onset => gentle attack, not a jump

(def ^:private ipc-sock "/tmp/smoke-mpv.sock")  ; mpv JSON IPC socket (pause/seek control)

(def ^:private player-opts
  {"mpv"    ["--no-video" "--no-terminal" "--really-quiet"]
   "ffplay" ["-nodisp" "-autoexit" "-loglevel" "quiet"]
   "paplay" []})

(defn- have? [cmd]
  (try (zero? (.waitFor (.start (ProcessBuilder. ^java.util.List ["sh" "-c" (str "command -v " cmd)]))))
       (catch Throwable _ false)))

(defn- player-cmd [path]
  (when-let [p (some #(when (have? %) %) ["mpv" "ffplay" "paplay"])]
    (into [p] (conj (cond-> (player-opts p)
                      (= p "mpv") (conj (str "--input-ipc-server=" ipc-sock)))  ; only mpv has IPC
                    path))))

(defn- mpv-cmd!
  "Send one JSON command line to the running mpv via its IPC socket. No-op
   (returns false) if the socket isn't there (e.g. ffplay/paplay fallback)."
  [json]
  (try
    (with-open [ch (java.nio.channels.SocketChannel/open java.net.StandardProtocolFamily/UNIX)]
      (.connect ch (java.net.UnixDomainSocketAddress/of ^String ipc-sock))
      (.write ch (java.nio.ByteBuffer/wrap (.getBytes (str json "\n") "UTF-8"))))
    true
    (catch Throwable _ false)))

(defn- tick! []
  (when-let [{:keys [analysis ^long t0]} @state]
    (let [secs  (/ (- (System/nanoTime) t0) 1.0e9)
          ^doubles gains (gains-at analysis secs)
          p     @core/params]
      (if gains
        (let [pal (or (:palette p) (:palette (scene/theme p)) [[1.0 1.0 1.0]])
              ;; gate the onset so only real beats fire, then sharpen to 0..1
              g   0.15
              fv  (max 0.0 (/ (- (flux-at analysis secs) g) (- 1.0 g)))
              ;; count beats (rising-edge trigger, debounced); accent every Nth one
              _   (when (and (not @armed) (> fv 0.30)) (reset! armed true) (swap! beat-n inc))
              _   (when (< fv 0.12) (reset! armed false))
              ;; every beat kicks at the base level; every Nth gets the stronger accent
              acc (if (zero? (mod @beat-n (long (:audio-beat-every p 4))))
                    (double (:audio-beat-accent p 3.0))
                    (double (:audio-beat-base p 2.0)))
              fv* (* fv acc)
              ;; beat: rise gently toward the (accented) onset, then decay — no hard jump
              k   (let [pk (double @kick)]
                    (max (* pk KICK-DECAY) (+ pk (* KICK-ATTACK (- fv* pk)))))
              ;; overall loudness (mean band gain) => emission boost (more density)
              n   (alength gains)
              energy (if (pos? n) (/ (areduce gains i s 0.0 (+ s (aget gains i))) n) 0.0)]
          (reset! kick k)
          (cond
            ;; agents mode: gains drive per-colour-group deposit in physarum; keep scalar,
            ;; NO global emit boost (the per-group bloom replaces it) => clean, not muddy
            (:audio-agents? p)
            (do (reset! scene/audio-gains gains)
                (reset! scene/audio-keep nil)
                (reset! scene/audio-emit nil))
            ;; white mode: agents fade white->colour from the raw band gains; keep scalar
            (:audio-white? p)
            (do (reset! scene/audio-gains gains)
                (reset! scene/audio-keep nil)
                (reset! scene/audio-emit (* (double (:audio-emit-amp p)) energy)))
            ;; default: per-channel keep colouring
            :else
            (do (reset! scene/audio-keep (channel-keep gains pal (:keep p) (:audio-amp p) (:audio-floor p)))
                (reset! scene/audio-gains nil)
                (reset! scene/audio-emit (* (double (:audio-emit-amp p)) energy))))
          (reset! scene/audio-dt (* (double (:audio-dt-amp p)) k)))
        (do (reset! scene/audio-keep nil)
            (reset! scene/audio-gains nil)
            (reset! scene/audio-dt nil)
            (reset! scene/audio-emit nil)
            (reset! kick 0.0))))))

(defn stop!
  "Stop playback and hand the smoke back to its scalar :keep."
  []
  (reset! scene/audio-hook nil)
  (when-let [{:keys [^Process proc]} @state]
    (try (.destroy proc) (catch Throwable _)))
  (reset! state nil)
  (reset! kick 0.0) (reset! beat-n 0) (reset! armed false)
  (reset! scene/audio-keep nil)
  (reset! scene/audio-dt nil)
  (reset! scene/audio-emit nil))

(defn start!
  "Analyse `path`, play it via an external player, and modulate per-colour keep
   (and dt on beats) from its spectrum. Buckets default to the active palette's
   colour count. Options:
     :amp   — override (:audio-amp params) for this run
     :bands — override the bucket count"
  [path & {:keys [amp bands]}]
  (stop!)
  (when amp (swap! core/params assoc :audio-amp amp))
  (let [p        @core/params
        nb       (or bands (max 1 (count (or (:palette p) (:palette (scene/theme p)) [1]))))
        analysis (analyze path :bands nb :fps 60)
        cmd      (or (player-cmd path)
                     (throw (ex-info "No external audio player found (mpv/ffplay/paplay)" {})))
        proc     (.start (doto (ProcessBuilder. ^java.util.List cmd)
                           (.redirectOutput ProcessBuilder$Redirect/DISCARD)
                           (.redirectError ProcessBuilder$Redirect/DISCARD)))
        t0       (System/nanoTime)]
    (reset! state {:proc proc :analysis analysis :t0 t0})
    ;; the render loop calls this once per frame => in lockstep, never starved
    (reset! scene/audio-hook (fn [] (try (tick!) (catch Throwable _))))
    {:dur-secs (:dur-secs analysis) :bands nb :rate (:rate analysis) :player (first cmd)}))

(defn restart-track!
  "Seek the audio back to the start and re-sync (wired to the sketch's 'r' key)."
  []
  (when @state
    (mpv-cmd! "{\"command\":[\"seek\",0,\"absolute\"]}")
    (mpv-cmd! "{\"command\":[\"set_property\",\"pause\",false]}")
    (swap! state assoc :t0 (System/nanoTime) :paused-at nil)
    (reset! kick 0.0) (reset! beat-n 0) (reset! armed false)))

(defn set-paused!
  "Pause/resume the audio together with the sim (wired to the sketch's space key).
   On resume, t0 is pushed forward by the paused duration so wall-clock stays in
   sync with playback."
  [paused?]
  (when-let [st @state]
    (mpv-cmd! (str "{\"command\":[\"set_property\",\"pause\"," (if paused? "true" "false") "]}"))
    (if paused?
      (swap! state assoc :paused-at (System/nanoTime))
      (when-let [pa (:paused-at st)]
        (swap! state (fn [s] (-> s (update :t0 + (- (System/nanoTime) (long pa)))
                                 (assoc :paused-at nil))))))))

(defn play-with-sim!
  "Open the sketch and start the audio together. Extra args go to `start!`."
  [path & opts]
  (core/start!)
  (apply start! path opts))

(comment
  (require '[smoke.audio :as a] :reload)
  (swap! smoke.core/params merge (smoke.scene/preset-params :tropic-rivers))
  (a/play-with-sim! "/tmp/song.wav")
  (swap! smoke.core/params assoc :audio-amp 0.05)
  (a/stop!))
