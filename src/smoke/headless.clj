(ns smoke.headless
  "Headless renderer — runs the smoke solver with NO Quil window and writes
   frames straight to PNG. For debugging/tuning without a display:

     clj -M -e \"(require 'smoke.headless)(smoke.headless/snap 120 \\\"/tmp/f.png\\\")\"

   From the nREPL:
     (smoke.headless/snap 120 \"/tmp/f.png\")             ; 120 steps -> one frame
     (smoke.headless/snap 120 \"/tmp/f.png\" {:visc 1e-4}) ; override params
     (smoke.headless/film 200 20 \"/tmp/frame\")           ; frame every 20 steps

   Same pipeline as smoke.core, minus the mouse."
  (:require [smoke.fluid :as f]
            [smoke.scene :as scene])
  (:import [java.awt.image BufferedImage DataBufferInt]
           [javax.imageio ImageIO]
           [java.io File]))

(defn step [fl p]
  (scene/seed-sources! fl p)
  (scene/advance fl p))

(defn render! [fl p path]
  (let [img (BufferedImage. (int scene/W) (int scene/W) BufferedImage/TYPE_INT_RGB)
        px  (.getData ^DataBufferInt (.getDataBuffer (.getRaster img)))]
    (scene/render-pixels! fl p px)
    (ImageIO/write img "png" (File. ^String path))
    path))

(defn snap
  "Run `steps` steps from a fresh field, then write one PNG. `overrides` merges
   over scene/default-params."
  ([steps path] (snap steps path nil))
  ([steps path overrides]
   (let [p  (merge scene/default-params overrides)
         fl (reduce (fn [acc _] (step acc p)) (f/make-fluid scene/N) (range steps))]
     (render! fl p path)
     {:path path :steps steps})))

(defn film
  "Run `steps` steps, writing a PNG every `every` steps to `<prefix>-NNNN.png`."
  ([steps every prefix] (film steps every prefix nil))
  ([steps every prefix overrides]
   (let [p (merge scene/default-params overrides)]
     (loop [fl (f/make-fluid scene/N) i 0 out []]
       (if (>= i steps)
         out
         (let [fl  (step fl p)
               out (if (zero? (mod (inc i) every))
                     (conj out (render! fl p (format "%s-%04d.png" prefix (inc i))))
                     out)]
           (recur fl (inc i) out)))))))
