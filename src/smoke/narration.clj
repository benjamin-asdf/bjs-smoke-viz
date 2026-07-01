(ns smoke.narration
  "Text-to-speech narration -> ducked ambient-bed mix, for *narrated* smoke
   videos (a spoken essay laid under the reactive visuals).

   Shells out to the bundled Piper neural TTS (tools/piper) and ffmpeg. The
   final mixed WAV is meant to be fed to `smoke.video/render!` as :audio — the
   :pulse? (vocal-band) engine then makes the smoke breathe with the speech.

   Pipeline:
     script (vector of {:text :gap}) --piper--> per-segment wavs
       --concat with silence gaps--> narration.wav (mono 22.05k)
       + procedural ambient drone of the same length
       --sidechain-duck the bed under the voice--> voiced-bed.wav (44.1k stereo)

   Quick start:
     (require '[smoke.narration :as narr] '[smoke.video :as v])
     (def b (narr/build! narr/soul-script \"/tmp/soul\"))   ; => {:mixed ... :seconds ..}
     (v/render! \"soul.mp4\" :audio (:mixed b) :preset :narration
                :seconds (long (Math/ceil (:seconds b))) :render [1920 1080])"
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str])
  (:import [java.io File]))

;; ---------------------------------------------------------------------------
;; bundled piper paths (downloaded into the repo, gitignored binaries/models)
;; ---------------------------------------------------------------------------

(def piper-dir   "tools/piper")
(def piper-bin   (str piper-dir "/piper"))
(def default-voice "assets/voices/en_GB-alan-medium.onnx")

(defn- shell!
  "Run argv, throwing with captured stderr on a non-zero exit. `env` extends
   the inherited environment (we need LD_LIBRARY_PATH for the piper libs)."
  [argv & {:keys [in env]}]
  (let [base   (into {} (System/getenv))
        result (apply sh/sh (concat argv
                                    [:env (merge base env)]
                                    (when in [:in in])))]
    (when-not (zero? (:exit result))
      (throw (ex-info (str "command failed: " (first argv))
                      {:argv argv :exit (:exit result) :err (:err result)})))
    result))

;; ---------------------------------------------------------------------------
;; TTS
;; ---------------------------------------------------------------------------

(defn tts!
  "Render `text` to a WAV at `out` with Piper. Returns `out`.
     :voice            path to a .onnx voice model
     :length-scale     >1 slows speech (gravitas), <1 speeds it up
     :sentence-silence seconds of silence Piper inserts between sentences"
  [text out & {:keys [voice length-scale sentence-silence]
               :or {length-scale 1.0 sentence-silence 0.4}}]
  (shell! [piper-bin
           "--model" (or voice default-voice)
           "--length_scale" (str length-scale)
           "--sentence_silence" (str sentence-silence)
           "--output_file" out]
          :in text
          :env {"LD_LIBRARY_PATH" piper-dir})
  out)

(defn audio-secs
  "Duration of an audio file in seconds (via ffprobe)."
  [path]
  (-> (shell! ["ffprobe" "-v" "error" "-show_entries" "format=duration"
               "-of" "csv=p=0" path])
      :out str/trim Double/parseDouble))

(defn- silence!
  "Write `secs` of silence to `out` matching piper's 22050/mono/s16 format."
  [secs out]
  (shell! ["ffmpeg" "-y" "-hide_banner" "-loglevel" "error"
           "-f" "lavfi" "-i" "anullsrc=r=22050:cl=mono"
           "-t" (format "%.3f" (double secs))
           "-c:a" "pcm_s16le" out])
  out)

(defn render-script!
  "Render a whole `script` (vector of {:text, optional :gap seconds, plus any
   per-segment tts! overrides}) into a single narration WAV at `out`.
   Returns {:path out :seconds <total>}."
  [script out & {:keys [voice length-scale sentence-silence gap]
                 :or {length-scale 1.06 sentence-silence 0.45 gap 0.7}}]
  (let [tmp   (File/createTempFile "narr" "")
        _     (.delete tmp)
        _     (.mkdirs tmp)
        dir   (.getPath tmp)
        parts (atom [])]
    (doseq [[i seg] (map-indexed vector script)
            :let [seg  (if (string? seg) {:text seg} seg)
                  text (str/trim (:text seg))]
            :when (seq text)]
      (let [wav (str dir "/seg-" (format "%03d" i) ".wav")]
        (tts! text wav
              :voice (:voice seg voice)
              :length-scale (:length-scale seg length-scale)
              :sentence-silence (:sentence-silence seg sentence-silence))
        (swap! parts conj wav)
        (let [g (:gap seg gap)]
          (when (pos? g)
            (let [s (str dir "/gap-" (format "%03d" i) ".wav")]
              (silence! g s)
              (swap! parts conj s))))))
    ;; concat all parts (same format) via the ffmpeg concat demuxer
    (let [list-file (str dir "/list.txt")]
      (spit list-file (str/join "\n" (map #(str "file '" % "'") @parts)))
      (shell! ["ffmpeg" "-y" "-hide_banner" "-loglevel" "error"
               "-f" "concat" "-safe" "0" "-i" list-file
               "-c" "copy" out]))
    {:path out :seconds (audio-secs out)}))

;; ---------------------------------------------------------------------------
;; ambient bed + ducked mix
;; ---------------------------------------------------------------------------

(defn make-drone!
  "Generate `secs` of a calm ambient pad (an open A-fifth stack with a slow
   breathing tremolo, warm low-pass and a long echo tail) at `out`.
   44.1k stereo. :root sets the low note in Hz (default A2)."
  [secs out & {:keys [root] :or {root 110.0}}]
  (let [f1 root, f2 (* root 1.5), f3 (* root 2.0)   ; root, fifth, octave
        sine (fn [f] (str "sine=frequency=" (format "%.2f" f) ":sample_rate=44100"))]
    (shell! ["ffmpeg" "-y" "-hide_banner" "-loglevel" "error"
             "-f" "lavfi" "-i" (sine f1)
             "-f" "lavfi" "-i" (sine f2)
             "-f" "lavfi" "-i" (sine f3)
             "-filter_complex"
             (str "[0]volume=0.5[a];[1]volume=0.32[b];[2]volume=0.22[c];"
                  "[a][b][c]amix=inputs=3:normalize=0[m];"
                  "[m]tremolo=f=0.1:d=0.6,lowpass=f=520,"
                  "aecho=0.8:0.7:550|830:0.4|0.3,volume=0.7[out]")
             "-map" "[out]" "-t" (format "%.3f" (double secs))
             "-ac" "2" "-ar" "44100" "-c:a" "pcm_s16le" out])
    out))

(defn mix!
  "Duck the ambient `bed` under the `narration` voice and mix to `out`
   (44.1k stereo). :bed-gain is the bed's resting level; the sidechain
   compressor dips it whenever the voice is present."
  [narration bed out & {:keys [bed-gain voice-gain]
                        :or {bed-gain 0.22 voice-gain 1.0}}]
  (shell! ["ffmpeg" "-y" "-hide_banner" "-loglevel" "error"
           "-i" narration "-i" bed
           "-filter_complex"
           (str "[0]aresample=44100,volume=" voice-gain ",pan=stereo|c0=c0|c1=c0[voc];"
                "[1]volume=" bed-gain "[bedraw];"
                "[bedraw][voc]sidechaincompress=threshold=0.015:ratio=8:attack=15:release=350[duck];"
                "[duck][voc]amix=inputs=2:duration=longest:normalize=0[mix]")
           "-map" "[mix]" "-ac" "2" "-ar" "44100" "-c:a" "pcm_s16le" out])
  out)

(defn build!
  "Full pipeline: script -> narration -> ambient bed -> ducked mix.
   `out-base` is a path prefix; writes <base>-voice.wav, <base>-bed.wav,
   <base>-mix.wav. Returns {:narration .. :bed .. :mixed .. :seconds ..}.
   :bed may be a path to your own ambient track (looped/cut to length) instead
   of the generated drone."
  [script out-base & {:keys [bed bed-gain] :as opts}]
  (let [voice  (str out-base "-voice.wav")
        bedwav (str out-base "-bed.wav")
        mixed  (str out-base "-mix.wav")
        {:keys [seconds]} (apply render-script! script voice
                                 (mapcat identity (dissoc opts :bed :bed-gain)))]
    (if bed
      ;; user bed: cut/loop to narration length
      (shell! ["ffmpeg" "-y" "-hide_banner" "-loglevel" "error"
               "-stream_loop" "-1" "-i" bed
               "-t" (format "%.3f" seconds)
               "-ac" "2" "-ar" "44100" "-c:a" "pcm_s16le" bedwav])
      (make-drone! seconds bedwav))
    (mix! voice bedwav mixed :bed-gain (or bed-gain 0.22))
    {:narration voice :bed bedwav :mixed mixed :seconds seconds}))

;; ---------------------------------------------------------------------------
;; the script — "the soul that unites us all" (DRAFT, English)
;; ---------------------------------------------------------------------------
;; Darwin's closing lines are verbatim (public domain, On the Origin of
;; Species, 1859). Dawkins / Deutsch / Braitenberg / Egan / Hof are
;; PARAPHRASED and attributed — Egan kept minimal (Permutation City is
;; copyrighted; this gestures at the "Dust Theory" rather than quoting it).

(def soul-script
  [;; --- tree of life ---
   {:text "Look closely at any living thing, and you will find the same machinery." :gap 0.9}
   {:text "The same four letters, written in one chemical alphabet, copied across four billion years." :gap 0.9}
   {:text "We are not separate creations. We are branches of a single tree." :gap 1.8}

   ;; --- Darwin (verbatim, public domain) ---
   {:text "Charles Darwin saw it first." :gap 0.8}
   {:text "It is interesting to contemplate a tangled bank, clothed with many plants of many kinds, with birds singing on the bushes, with various insects flitting about, and with worms crawling through the damp earth." :gap 1.0}
   {:text "And to reflect that these elaborately constructed forms, so different from each other, have all been produced by laws acting around us." :gap 1.2}
   {:text "There is grandeur in this view of life. From so simple a beginning, endless forms most beautiful and most wonderful have been, and are being, evolved." :gap 1.8}

   ;; --- Dawkins (paraphrase) ---
   {:text "Richard Dawkins turned the lens around. Do not look at the creature. Look at the gene." :gap 0.9}
   {:text "We are vessels, built and steered by the molecules we carry forward." :gap 0.9}
   {:text "Life is a river of information flowing out of the deep past, and we are its living edge." :gap 1.8}

   ;; --- David Deutsch (paraphrase) ---
   {:text "David Deutsch asks a different question. Not only where we came from, but what we can become." :gap 0.9}
   {:text "We are explainers. With the right knowledge, anything not forbidden by the laws of nature can be achieved." :gap 0.9}
   {:text "This is the beginning of infinity." :gap 1.8}

   ;; --- Braitenberg (paraphrase) ---
   {:text "Valentino Braitenberg built tiny vehicles. Two sensors, two wheels, a few crossed wires." :gap 0.9}
   {:text "And watching them move, you would swear they felt fear, and aggression, and even love." :gap 0.9}
   {:text "Complexity is cheap. The mind may be simpler than we fear, and far stranger than we hope." :gap 1.8}

   ;; --- Greg Egan / Permutation City (minimal, attributed) ---
   {:text "Greg Egan pushed the thought to its edge." :gap 0.8}
   {:text "If you are a pattern, a process and not a substance, then the dust of the universe, rearranged, could carry you just as well as flesh." :gap 1.0}
   {:text "The self is a pattern that does not care what it is written on." :gap 1.8}

   ;; --- Wim Hof (paraphrase) — the unifying thread ---
   {:text "And then, beneath all of it, there is the breath." :gap 0.9}
   {:text "Wim Hof says the cold is a teacher, and the breath is a doorway." :gap 0.9}
   {:text "Below the thinking mind, below the genes and the patterns, one thing is shared by everything that lives." :gap 0.9}
   {:text "The breath, going in and out, the same in every living chest." :gap 1.2}
   {:text "This is the soul that unites us all." :gap 2.2}

   ;; --- close ---
   {:text "One tree. One river. One pattern. One breath." :gap 1.0}
   {:text "Endless forms, most beautiful." :gap 1.5}])
