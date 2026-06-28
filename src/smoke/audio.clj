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
            [smoke.scene :as scene]
            [smoke.physarum :as phys])
  (:import [javax.sound.sampled AudioSystem AudioFormat AudioFormat$Encoding]
           [org.jtransforms.fft DoubleFFT_1D]
           [java.io File]
           [java.lang ProcessBuilder$Redirect]))

(set! *warn-on-reflection* true)

;; ---- analysis --------------------------------------------------------------
(defn- band-edges
  "Log-spaced FFT-bin edges for `bands` buckets from ~40 Hz to Nyquist."
  [bands fft-n rate]
  (let [lo 40.0 hi (/ (double rate) 2.0) bins-per-hz (/ (double fft-n) (double rate))]
    (mapv (fn [b]
            (long (* bins-per-hz (* lo (Math/pow (/ hi lo) (/ (double b) (double bands)))))))
          (range (inc bands)))))

(defn analyze
  "Stream-analyse `path` (WAV/AIFF) into {:hops [double[bands] ...] :flux ...
   :hop-secs :bands :rate :dur-secs}. Reads the file in ONE pass through a ring
   buffer of the last `fft-n` samples — only that window is held in memory, so
   arbitrarily long files (multi-hour sets) analyse without loading the whole
   track. Each band gain is normalised to 0..1 over the track; `fps` should
   match the render frame rate."
  [path & {:keys [bands fps fft-n] :or {bands 3 fps 60 fft-n 2048}}]
  (with-open [in0 (AudioSystem/getAudioInputStream (File. ^String path))]
    (let [^javax.sound.sampled.AudioFormat base (.getFormat in0)
          rate   (double (.getSampleRate base))
          ch     (long (.getChannels base))
          target (AudioFormat. AudioFormat$Encoding/PCM_SIGNED (float rate) 16 (int ch)
                               (int (* 2 ch)) (float rate) false)
          ^javax.sound.sampled.AudioInputStream in (AudioSystem/getAudioInputStream target in0)
          fftn   (long fft-n)
          hop    (max 1 (long (/ rate (double fps))))
          fft    (DoubleFFT_1D. fftn)
          edges  (band-edges bands fftn rate)
          half   (quot fftn 2)
          hann   (double-array fftn)
          ring   (double-array fftn)       ; circular window of last fftn mono samples
          win    (double-array fftn)
          raw    (java.util.ArrayList.)    ; per-hop double[bands], grows with the track
          frameb (int (* 2 ch))
          chunk  (byte-array (* frameb 16384))
          emit!  (fn [^long widx]          ; FFT the current window (oldest at widx)
                   (dotimes [i fftn]
                     (aset win i (* (aget ring (rem (+ widx i) fftn)) (aget hann i))))
                   (.realForward fft win)
                   (let [g (double-array bands)]
                     (dotimes [b bands]
                       (let [b0 (max 1 (long (nth edges b)))
                             b1 (min half (long (nth edges (inc b))))]
                         (aset g b (double (loop [k b0 acc 0.0]
                                             (if (< k b1)
                                               (let [re (aget win (* 2 k)) im (aget win (inc (* 2 k)))]
                                                 (recur (inc k) (+ acc (Math/sqrt (+ (* re re) (* im im))))))
                                               (/ acc (double (max 1 (- b1 b0))))))))))
                     (.add raw g)))]
      (dotimes [i fftn]
        (aset hann i (* 0.5 (- 1.0 (Math/cos (/ (* 2.0 Math/PI i) (dec fftn)))))))
      ;; stream: read frame-aligned chunks (carry partial frames), decode to mono,
      ;; push into the ring, emit a hop every `hop` samples
      (let [total
            (loop [carry 0 widx 0 total 0]
              (let [n (.read in chunk carry (- (alength chunk) carry))]
                (if (neg? n)
                  total
                  (let [avail   (+ carry n)
                        nframes (quot avail frameb)
                        used    (* nframes frameb)
                        res     (loop [f 0 widx widx total total]
                                  (if (< f nframes)
                                    (let [bb (* f frameb)
                                          s  (loop [c 0 acc 0]
                                               (if (< c ch)
                                                 (let [pp (+ bb (* 2 c))
                                                       lo (bit-and (aget chunk pp) 0xff)
                                                       hi (aget chunk (inc pp))]
                                                   (recur (inc c) (+ acc (bit-or (bit-shift-left hi 8) lo))))
                                                 acc))
                                          mono   (/ (double s) (* (double ch) 32768.0))
                                          widx'  (rem (inc (long widx)) fftn)
                                          total' (inc (long total))]
                                      (aset ring (long widx) mono)
                                      (when (zero? (rem total' hop)) (emit! widx'))
                                      (recur (inc f) widx' total'))
                                    [widx total]))
                        rem-bytes (- avail used)]
                    (when (pos? rem-bytes) (System/arraycopy chunk used chunk 0 rem-bytes))
                    (recur rem-bytes (long (nth res 0)) (long (nth res 1)))))))
            nhops (.size raw)]
        ;; normalise each band by its peak; onset flux = positive band-energy rise
        (let [maxes (double-array bands)
              flux  (double-array nhops)]
          (dotimes [h nhops]
            (let [^doubles g (.get raw h)]
              (dotimes [b bands] (when (> (aget g b) (aget maxes b)) (aset maxes b (aget g b))))
              (when (pos? h)
                (let [^doubles gp (.get raw (dec h))]
                  (aset flux h (double (reduce (fn [a b] (+ (double a) (max 0.0 (- (aget g b) (aget gp b)))))
                                               0.0 (range bands))))))))
          (when (pos? nhops)
            (let [sorted (double-array flux)
                  _      (java.util.Arrays/sort sorted)
                  p95    (aget sorted (long (* 0.95 (dec nhops))))
                  norm   (if (pos? p95) p95 1.0)]
              (dotimes [h nhops] (aset flux h (min 1.0 (/ (aget flux h) norm))))))
          (dotimes [h nhops]                         ; normalise band gains in place
            (let [^doubles g (.get raw h)]
              (dotimes [b bands] (let [m (aget maxes b)] (aset g b (if (pos? m) (/ (aget g b) m) 0.0))))))
          {:hops     (vec raw)
           :flux     flux
           :hop-secs (/ 1.0 (double fps))
           :bands    bands
           :rate     rate
           :dur-secs (/ (double total) rate)})))))

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

(defn- vivid-palette
  "Seeded random vivid palette — random base hue + random arc span (+ jitter) from
   `seed`, so each seed gives a visibly DIFFERENT-looking set, not just a rotated
   full wheel. Shares the logic with the live `:p-rand-color?` path."
  [^long seed ^long n]
  (phys/vivid-palette* (java.util.Random. seed) n))

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
(defonce ^:private puff-armed (atom false)) ; same debounce for the (lower) puff onset trigger
(defonce ^:private color-seed (atom 0)) ; RNG seed behind the current colour shuffle (re-rolled on 'r')

(defn- base-palette [p]
  (or (:palette p) (:palette (scene/theme p)) [[1.0 1.0 1.0]]))

(defn- color-count
  "How many random agent hues to roll — decoupled from the audio band count so
   the palette can be richer (more colour variety) than the few keep-modulation
   bands. From `:audio-colors` (default 7)."
  ^long [p] (max 1 (long (:audio-colors p 7))))

(defn- tone-palette
  "Tone a palette's MOOD so it isn't only full-neon: `sat` scales chroma toward
   the per-colour grey mean (1 = full, lower = muted), then `lift` blends toward
   WHITE (0 = none, higher = pastel/washed). Keeps the hues, changes the feel."
  [pal ^double sat ^double lift]
  (mapv (fn [c]
          (let [m (/ (double (reduce + c)) 3.0)]
            (mapv (fn [ch]
                    (let [s (+ m (* sat (- (double ch) m)))]
                      (-> (+ s (* lift (- 1.0 s))) (max 0.0) (min 1.0))))
                  c)))
        pal))

(defn- reroll-colors!
  "Pick a fresh random seed and publish a new set of `nb` vivid hues into
   scene/audio-palette, toned by the live :audio-sat / :audio-lift mood (so it's
   not only neon), so the next agents built from it (every 'r', and offline
   renders) wear a freshly generated colour set."
  [^long nb]
  (let [seed (.nextLong (java.util.Random.))
        p    @core/params
        sat  (double (:audio-sat p 1.0)) lift (double (:audio-lift p 0.0))
        ;; curated colour set if chosen, else freshly generated random vivid hues
        base (or (get scene/audio-palettes (:audio-palette-set p))
                 (vivid-palette seed nb))]
    (reset! color-seed seed)
    (reset! scene/audio-palette (tone-palette base sat lift))
    seed))

(defn- recolor-live!
  "Repaint the agents in the running sketch from the current hue palette, so the
   new colours show immediately (start!/restart-track! don't rebuild the agents)."
  []
  (when-not (or (:p-rand-color? @core/params)   ; rand-color agents keep their own random hues
                (:audio-puffs? @core/params))    ; :puffs agents stay white (colour lives in the puffs)
    (when-let [ph (:phys @core/last-state)]
      (when-let [pal @scene/audio-palette]
        (phys/recolor! ph pal)))))

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

(defn- voice-score
  "Heuristic 'vocal presence' (0..1) from band `gains`: energy concentrated in the
   mid/formant region (~250 Hz–3 kHz) RELATIVE to the bass + treble around it. NOT
   true vocal isolation — a midrange-dominance detector — but it tracks sung/spoken
   passages well enough to drive a centre glow. The vocal band is :voice-band-lo ..
   :voice-band-hi as FRACTIONS of the spectrum; :voice-contrast is how strongly the
   surrounding bass/treble suppress the score (higher => more selective)."
  ^double [^doubles gains p]
  (let [n   (alength gains)
        lo  (max 0 (long (* (double (:voice-band-lo p 0.2)) n)))
        hi  (min n (long (max (inc lo) (long (* (double (:voice-band-hi p 0.65)) n)))))
        meanr (fn ^double [^long a ^long b]
                (if (< a b)
                  (loop [i a s 0.0] (if (< i b) (recur (inc i) (+ s (aget gains i))) (/ s (double (- b a)))))
                  0.0))
        mid   (meanr lo hi)
        below (meanr 0 lo)
        above (meanr hi n)
        c     (double (:voice-contrast p 0.6))]
    (-> (- mid (* c 0.5 (+ below above))) (max 0.0) (min 1.0))))

(defn- push-puff!
  "Compute one beat puff from the spectrum and queue it into scene/audio-puffs.
   DETERMINISTIC from `gains`:
     angle  = spectral centroid (gain-weighted mean band) mapped around a full
              circle => bass-heavy onsets fire one way, bright/trebly onsets
              another (a 'radial clock'); the puff shoots outward along it.
     colour = the dominant (loudest) band's hue from `pal`.
     size / amount / speed scale with onset strength `fv` (0..1).
   `pal` is the active vivid-hue palette (one colour per band)."
  [^doubles gains pal ^double fv p]
  (let [n   (alength gains)
        sum (areduce gains i s 0.0 (+ s (aget gains i)))
        cen (if (pos? sum)
              (/ (areduce gains i s 0.0 (+ s (* (double i) (aget gains i)))) sum)
              (* 0.5 n))                                   ; silence => straight up
        dom (loop [i 1 bi 0 bv (aget gains 0)]             ; argmax band => colour
              (if (< i n)
                (if (> (aget gains i) bv) (recur (inc i) i (aget gains i)) (recur (inc i) bi bv))
                bi))
        ;; map the dominant band onto the palette by FRACTION, so the palette size
        ;; (agent hue count) and the band count can differ freely
        palv (vec pal)
        pc   (count palv)
        col  (if (pos? pc)
               (nth palv (min (dec pc) (long (* (/ (double dom) (double (max 1 n))) pc))))
               [1.0 1.0 1.0])
        ang  (+ (double (:puff-angle p 0.0))
                (* scene/TAU (/ cen (double (max 1 n)))))
        dx   (Math/cos ang) dy (Math/sin ang)
        sr   (double (:puff-spawn-r p 0.05))
        st   (min 1.0 (max 0.0 fv))
        rad  (* (double (:puff-radius p 7.0)) (+ 0.55 (* 0.45 st)))
        amt  (* (double (:puff-amount p 2.6)) (+ 0.40 (* 0.60 st)))
        vel  (* (double (:puff-vel p 7.0))    (+ 0.40 (* 0.60 st)))]
    (swap! scene/audio-puffs conj
           {:x (+ 0.5 (* sr dx)) :y (+ 0.5 (* sr dy))
            :color col :r rad :amount amt
            :vx (* vel dx) :vy (* vel dy)})))

(defn- push-spectral-puffs!
  "Continuous spectral emission (NO threshold / no onset): EVERY frequency band
   emits a puff each call, sized + sped by its CURRENT gain. Each band sits at its
   own fixed clock angle, so the spectrum fans out as a ring of coloured puffs —
   the louder a band, the more it puffs and the further it shoots. Runs every
   frame, so `:puff-spectral-scale` throttles the per-frame deposit."
  [^doubles gains pal p]
  (let [n    (alength gains)
        palv (vec pal) pc (count palv)
        off  (double (:puff-angle p 0.0))
        sr   (double (:puff-spawn-r p 0.28))
        brad (double (:puff-radius p 7.0))
        bamt (* (double (:puff-amount p 2.6)) (double (:puff-spectral-scale p 0.15)))
        bvel (double (:puff-vel p 7.0))
        ;; 1:1 freq -> smoke, but lift the QUIET end so soft tones still show:
        ;; ge = g^gamma (gamma<1 boosts low gains; 1.0 = pure linear)
        gam  (double (:puff-gain-gamma p 1.0))]
    (dotimes [b n]
      (let [g (aget gains b)]
        (when (> g 0.003)                     ; skip ~silent bands (perf; no real threshold)
          (let [ge  (if (== gam 1.0) g (Math/pow g gam))
                ang (+ off (* scene/TAU (/ (+ (double b) 0.5) (double (max 1 n)))))
                dx  (Math/cos ang) dy (Math/sin ang)
                col (if (pos? pc)
                      (nth palv (min (dec pc) (long (* (/ (double b) (double (max 1 n))) pc))))
                      [1.0 1.0 1.0])]
            (swap! scene/audio-puffs conj
                   {:x (+ 0.5 (* sr dx)) :y (+ 0.5 (* sr dy))
                    :color col :r (* brad (+ 0.5 (* 0.5 ge))) :amount (* bamt ge)
                    :vx (* bvel ge dx) :vy (* bvel ge dy)})))))))

(defn modulate!
  "Apply one frame of audio modulation for playback time `secs`: set the scene's
   per-colour keep / dt / emit / gains atoms from the analysis at that time. Pure
   w.r.t. wall-clock — drives both the live tick (secs = elapsed) and the offline
   video renderer (secs = frame/fps), so a rendered file matches what you hear.
   Advances the beat-counter/kick envelope atoms, so call once per frame in order."
  [analysis secs p]
  (let [secs (double secs)
        ^doubles gains (gains-at analysis secs)]
    (if gains
      (let [pal (or @scene/audio-palette (base-palette p))  ; same hues the agents wear

              ;; gate the onset so only real beats fire, then sharpen to 0..1
            g   0.15
            fv  (max 0.0 (/ (- (flux-at analysis secs) g) (- 1.0 g)))
              ;; count beats (rising-edge trigger, debounced); accent every Nth one
            _   (when (and (not @armed) (> fv 0.30))
                  (reset! armed true) (swap! beat-n inc) (swap! scene/beat-count inc)
                  ;; colour-cycle: every Nth beat roll a fresh palette (puffs read it
                  ;; live; advance repaints the agents via scene/recolor-pending?)
                  (let [cyc (long (:audio-color-cycle p 0))]
                    (when (and (pos? cyc) (zero? (mod @beat-n cyc)))
                      (reroll-colors! (color-count p))
                      (reset! scene/recolor-pending? true))))
            _   (when (< fv 0.12) (reset! armed false))
              ;; puffs: :puff-continuous? => EVERY band puffs each frame from its
              ;; current gain (no threshold, the spectrum itself makes the puffs);
              ;; else a single onset-triggered puff with its own (lower) threshold
            pt  (double (:puff-thresh p 0.18))
            _   (when (:audio-puffs? p)
                  (if (:puff-continuous? p)
                    (push-spectral-puffs! gains pal p)
                    (cond
                      (and (not @puff-armed) (> fv pt))
                      (do (reset! puff-armed true) (push-puff! gains pal fv p))
                      (< fv (* 0.6 pt)) (reset! puff-armed false))))
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
            energy (if (pos? n) (/ (areduce gains i s 0.0 (+ s (aget gains i))) n) 0.0)
              ;; compress loudness -> emission: a floor keeps smoke flowing in
              ;; quiet passages, and (1-floor) caps how much louder peaks add, so
              ;; density never collapses when soft nor balloons when loud.
            ef  (double (:audio-emit-floor p 0.0))
            emit (* (double (:audio-emit-amp p)) (+ ef (* (- 1.0 ef) energy)))]
        (reset! kick k)
        (cond
            ;; puffs mode: all reactivity is in the beat puffs (queued above); the
            ;; agents stay a faint static white haze => no keep/emit modulation
          (:audio-puffs? p)
          (do (reset! scene/audio-keep nil)
              (reset! scene/audio-gains gains)   ; expose gains so emit-voice! can pick a shape by freq
              (reset! scene/audio-emit nil))
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
              (reset! scene/audio-emit emit))
            ;; default: per-channel keep colouring
          :else
          (do (reset! scene/audio-keep (channel-keep gains pal (:keep p) (:audio-amp p) (:audio-floor p)))
              (reset! scene/audio-gains nil)
              (reset! scene/audio-emit emit)))
        (reset! scene/audio-dt (* (double (:audio-dt-amp p)) k))
          ;; wind surges with the beat (kick) and stays up while loud (energy)
        (reset! scene/audio-wind (+ (* (double (:audio-wind-amp p 0.0)) k)
                                    (* (double (:audio-wind-energy p 0.0)) energy)))
          ;; heuristic vocal presence => white centre glow (scene/emit-voice!)
        (reset! scene/audio-voice (when (:voice? p) (voice-score gains p))))
      (do (reset! scene/audio-keep nil)
          (reset! scene/audio-gains nil)
          (reset! scene/audio-dt nil)
          (reset! scene/audio-emit nil)
          (reset! scene/audio-wind nil)
          (reset! scene/audio-voice nil)
          (reset! kick 0.0)))))

(defn- tick! []
  (when-let [{:keys [analysis ^long t0]} @state]
    (modulate! analysis (/ (- (System/nanoTime) t0) 1.0e9) @core/params)))

;; ---- offline (deterministic, frame-indexed) modulation ---------------------
;; Used by smoke.video to drive the same audio reactivity from a frame counter
;; instead of the wall clock, so a rendered file is perfectly in sync.
(defn offline-init!
  "Reset the beat/kick state and roll a fresh agent palette for an offline render
   of `analysis` (colour count from `p`'s :audio-colors). Clears the live
   audio-hook so nothing competes."
  [analysis p]
  (reset! scene/audio-hook nil)
  (reset! kick 0.0) (reset! beat-n 0) (reset! armed false) (reset! puff-armed false) (reset! scene/beat-count 0)
  (reset! scene/audio-puffs [])
  (reset! scene/audio-wind nil)
  (reset! scene/audio-voice nil)
  (reroll-colors! (color-count p)))

(defn band-count
  "Number of FFT frequency bands for `p`. Defaults to the active palette's colour
   count (each band drives its colour's keep), but `:audio-bands` overrides it —
   the :puffs theme needs fine spectral resolution (centroid/dominant band) even
   though its agents are a single white colour."
  [p]
  (max 1 (long (:audio-bands p (count (or (:palette p) (:palette (scene/theme p)) [1]))))))

(defn stop!
  "Stop playback and hand the smoke back to its scalar :keep."
  []
  (reset! scene/audio-hook nil)
  (when-let [{:keys [^Process proc]} @state]
    (try (.destroy proc) (catch Throwable _)))
  (reset! state nil)
  (reset! kick 0.0) (reset! beat-n 0) (reset! armed false) (reset! puff-armed false) (reset! scene/beat-count 0)
  (reset! scene/audio-puffs [])
  (reset! scene/audio-palette nil)  ; back to the theme's own palette
  (reset! scene/audio-keep nil)
  (reset! scene/audio-dt nil)
  (reset! scene/audio-emit nil)
  (reset! scene/audio-wind nil)
  (reset! scene/audio-voice nil))

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
        nb       (or bands (band-count p))
        analysis (analyze path :bands nb :fps 60)
        cmd      (or (player-cmd path)
                     (throw (ex-info "No external audio player found (mpv/ffplay/paplay)" {})))
        proc     (.start (doto (ProcessBuilder. ^java.util.List cmd)
                           (.redirectOutput ProcessBuilder$Redirect/DISCARD)
                           (.redirectError ProcessBuilder$Redirect/DISCARD)))
        t0       (System/nanoTime)]
    (reroll-colors! (color-count p))
    (recolor-live!)
    ;; agents built before the palette was rolled carry only band-count groups;
    ;; rebuild next frame so they spread across ALL the rolled hues (not just 3).
    (reset! core/reset? true)
    (reset! state {:proc proc :analysis analysis :t0 t0})
    ;; the render loop calls this once per frame => in lockstep, never starved
    (reset! scene/audio-hook (fn [] (try (tick!) (catch Throwable _))))
    {:dur-secs (:dur-secs analysis) :bands nb :rate (:rate analysis) :player (first cmd)}))

(defn restart-track!
  "Seek the audio back to the start and re-sync (wired to the sketch's 'r' key)."
  []
  (when-let [st @state]
    (mpv-cmd! "{\"command\":[\"seek\",0,\"absolute\"]}")
    (mpv-cmd! "{\"command\":[\"set_property\",\"pause\",false]}")
    (swap! state assoc :t0 (System/nanoTime) :paused-at nil)
    (reroll-colors! (color-count @core/params))     ; fresh random colours on every restart
    (recolor-live!)                                  ; repaint now (the 'r' rebuild also re-applies it)
    (reset! kick 0.0) (reset! beat-n 0) (reset! armed false) (reset! puff-armed false) (reset! scene/beat-count 0)))

(defn reseed-colors!
  "Roll a fresh random agent palette WITHOUT seeking the track — the 'Reset field'
   button path, so a field reset gives new random colours like 'r' does. No-op when
   no audio is active (non-audio :p-rand-color? rerolls itself in scene/new-fluid)."
  []
  (when @state
    (reroll-colors! (color-count @core/params))
    (recolor-live!)))

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

(defn- now-secs
  "Current playback position (seconds) from the wall-clock state; frozen at
   :paused-at while paused so a seek-while-paused stays correct."
  ^double [{:keys [^long t0 paused-at]}]
  (/ (- (long (or paused-at (System/nanoTime))) t0) 1.0e9))

(defn seek!
  "Seek the audio by `delta` seconds (relative; negative rewinds), keeping the
   modulation clock in sync so the smoke jumps WITH the sound. We re-anchor t0
   from the current position and tell mpv the absolute target, so the two never
   drift. Clamped at 0 (mpv clamps the upper end); a no-op if nothing's playing.
   Wired to the sketch's arrow keys."
  [delta]
  (when-let [st @state]
    (let [pos (max 0.0 (+ (now-secs st) (double delta)))
          now (System/nanoTime)
          t0  (long (- now (long (* pos 1.0e9))))]
      (mpv-cmd! (str "{\"command\":[\"seek\"," pos ",\"absolute\"]}"))
      (swap! state assoc
             :t0 t0
             ;; if paused, re-anchor the freeze point so elapsed stays = pos
             :paused-at (when (:paused-at st) now))
      pos)))

(defn play-with-sim!
  "Open the sketch and start the audio together. Extra args go to `start!`."
  [path & opts]
  (core/start!)
  (apply start! path opts))

(comment
  (require '[smoke.audio :as a] :reload)
  (swap! smoke.core/params merge (smoke.scene/preset-params :tropic-rivers))

  (a/play-with-sim! "/tmp/song.wav")
  (a/play-with-sim! "/home/benj/repos/musicanalysis/Alle Warten [Qdll_yQEtwQ].wav")

  (a/play-with-sim! "/home/benj/repos/musicanalysis/vom Feisten @ 44 Hertz at Kater Berlin - 12⧸12⧸25 [c7bSbvAHbMc].wav")
  (swap! smoke.core/params assoc :audio-amp 0.3)
  (swap! smoke.core/params assoc :audio-dt-amp 0.2)
  (:keep @smoke.core/params)
  0.9186
  (:visc @smoke.core/params)
  0.03

  (a/play-with-sim! "/home/benj/repos/musicanalysis/thamcut.wav")

  (a/play-with-sim! "/home/benj/repos/musicanalysis/such-a-mess-kyle-watson-bass-dub.wav")
  (a/play-with-sim! "/home/benj/repos/musicanalysis/d-neuland-vom-feisten-i-chaos.wav")
  (a/play-with-sim!
   "/home/benj/repos/musicanalysis/aldebara3min.wav")

  ;; ── how to start the PUFFS version (spectral puffs + slime flow + voice) ──
  (swap! smoke.core/params merge (smoke.scene/preset-params :puffs))
  (a/play-with-sim! "/home/benj/repos/musicanalysis/aldebara.wav")     ; full song
  (a/play-with-sim! "/home/benj/repos/musicanalysis/alicante.wav")     ; Boris Brejcha — Alicante

  ;; ── voice centre modes (live-switch; needs vocals/onsets to fire) ──
  (swap! smoke.core/params merge {:voice-amount 2.5 :voice-radius 0.3 :voice-random? false :voice-agents? false :voice-ring 0.0 :voice-bloom 0.0}) ; POINT
  (swap! smoke.core/params merge {:voice-amount 2.0 :voice-radius 0.6 :voice-random? true  :voice-agents? false :voice-ring 0.0 :voice-bloom 0.0}) ; SCATTER
  (swap! smoke.core/params merge {:voice-amount 0.0 :voice-random? false :voice-agents? false :voice-ring 11.0 :voice-bloom 0.0})                   ; RING
  (swap! smoke.core/params merge {:voice-amount 0.0 :voice-random? false :voice-agents? true :voice-agent-count 130 :voice-ring 0.0 :voice-bloom 0.0}) ; AGENTS
  (swap! smoke.core/params merge {:voice-amount 0.0 :voice-random? false :voice-agents? false :voice-ring 0.0 :voice-bloom 1.0})                    ; BLOOM
  (swap! smoke.core/params merge {:voice-amount 1.5 :voice-radius 0.3 :voice-random? true :voice-agents? true :voice-ring 7.0 :voice-bloom 0.5})    ; ALL

  ;; audio palette: random by default; pick a curated set then re-roll
  (swap! smoke.core/params assoc :audio-palette-set :sunset) (a/reseed-colors!)  ; :ice :ember :forest :ocean :sepia :pastel :candy :autumn / nil=random

  (a/stop!))
