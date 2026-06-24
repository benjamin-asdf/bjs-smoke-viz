(ns smoke.audio
  "Audio-reactive layer. Pre-analyse a WAV/AIFF file into a per-hop timeline of
   frequency-band gains (reusing the JTransforms FFT), then play the clip and,
   on a 60 Hz timer, modulate the smoke's per-colour `keep` from the live band
   energy.

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
  (:import [javax.sound.sampled AudioSystem AudioFormat AudioFormat$Encoding Clip]
           [org.jtransforms.fft DoubleFFT_1D]
           [java.io File]
           [java.util Timer TimerTask]))

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
    ;; normalise each band over the track by its peak
    (let [maxes (double-array bands)]
      (dotimes [h nhops]
        (let [^doubles g (aget raw h)]
          (dotimes [b bands] (when (> (aget g b) (aget maxes b)) (aset maxes b (aget g b))))))
      {:hops     (mapv (fn [^doubles g]
                         (double-array (map (fn [b] (let [m (aget maxes b)]
                                                      (if (pos? m) (/ (aget g b) m) 0.0)))
                                            (range bands))))
                       raw)
       :hop-secs (/ 1.0 (double fps))
       :bands    bands
       :rate     rate
       :dur-secs (/ (double nsamp) rate)})))

;; ---- per-frame modulation --------------------------------------------------
(defn- channel-keep
  "Per-channel keep [kr kg kb] from band `gains` and the active `palette`.
   Each band's gain is weighted by its palette colour's RGB content, so a loud
   band raises the keep of the channels that make up its colour."
  [^doubles gains palette base amp]
  (let [pal (vec palette)
        n   (min (count pal) (alength gains))]
    (mapv (fn [ch]
            (let [num (reduce (fn [a i] (+ a (* (aget gains i) (double (nth (nth pal i) ch))))) 0.0 (range n))
                  den (reduce (fn [a i] (+ a (double (nth (nth pal i) ch)))) 0.0 (range n))
                  contrib (if (pos? den) (/ num den) 0.0)]
              (min 0.999 (+ (double base) (* (double amp) contrib)))))
          [0 1 2])))

(defn- gains-at
  "Band gains at playback time `secs`, or nil past the end."
  [{:keys [hops hop-secs]} secs]
  (let [idx (long (/ (double secs) (double hop-secs)))]
    (when (< idx (count hops)) (nth hops idx))))

;; ---- player ----------------------------------------------------------------
(defonce ^:private state (atom nil))  ; {:clip Clip :analysis {..} :timer Timer}

(defn- tick! []
  (when-let [{:keys [^Clip clip analysis]} @state]
    (let [secs  (/ (.getMicrosecondPosition clip) 1.0e6)
          gains (gains-at analysis secs)]
      (if gains
        (let [p   @core/params
              pal (or (:palette p) (:palette (scene/theme p)) [[1.0 1.0 1.0]])]
          (reset! scene/audio-keep (channel-keep gains pal (:keep p) (:audio-amp p))))
        (reset! scene/audio-keep nil)))))

(defn stop!
  "Stop playback and hand the smoke back to its scalar :keep."
  []
  (when-let [{:keys [^Clip clip ^Timer timer]} @state]
    (when timer (.cancel timer))
    (try (.stop clip) (.close clip) (catch Exception _)))
  (reset! state nil)
  (reset! scene/audio-keep nil))

(defn start!
  "Analyse + play `path`, modulating per-colour keep from its spectrum. Buckets
   default to the active palette's colour count. Options:
     :amp   — override (:audio-amp params) for this run
     :bands — override the bucket count"
  [path & {:keys [amp bands]}]
  (stop!)
  (when amp (swap! core/params assoc :audio-amp amp))
  (let [p        @core/params
        nb       (or bands (max 1 (count (or (:palette p) (:palette (scene/theme p)) [1]))))
        analysis (analyze path :bands nb :fps 60)
        clip     (AudioSystem/getClip)]
    (with-open [in (AudioSystem/getAudioInputStream (File. ^String path))]
      (.open clip in))
    (let [timer (Timer. "smoke-audio-tick" true)]
      (.scheduleAtFixedRate timer
                            (proxy [TimerTask] [] (run [] (try (tick!) (catch Throwable _))))
                            (long 0) (long 16))
      (reset! state {:clip clip :analysis analysis :timer timer}))
    (.start clip)
    {:dur-secs (:dur-secs analysis) :bands nb :rate (:rate analysis)}))

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
