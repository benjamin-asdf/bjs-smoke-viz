(ns smoke.core
  "Quil front-end for the spectral smoke solver. Functional (fun-mode) sketch;
   the simulation + render live in smoke.scene (Quil-free), this is just the
   window, input, and the render loop.

   A buoyant smoke blob rises from the lower-left and drifts up toward the right
   ('window'). The solver is FFT-based (smoke.fluid) — smooth swirls, no
   checkerboard. The domain is periodic, so smoke faintly wraps edge-to-edge.

   LIVE CODING: knobs are in `params`, handlers are #'vars:
     (swap! smoke.core/params assoc :buoy 1.8)   ; tweak, applies next frame
     (smoke.core/start!)                          ; relaunch + reset
     (smoke.core/save-frame! \"/tmp/f.png\")        ; dump the rendered frame
   Interact: drag mouse to push smoke. space = pause, r = reset."
  (:require [quil.core :as q]
            [quil.middleware :as qm]
            [smoke.fluid :as f]
            [smoke.scene :as scene])
  (:import [processing.core PImage])
  (:gen-class))

(def params (atom scene/default-params))
(defonce sketch     (atom nil))   ; running applet
(defonce last-state (atom nil))   ; latest sim state, for REPL inspection

(defn setup []
  (q/frame-rate 60)
  (q/image-mode :corner)
  (assoc (f/make-fluid scene/N)
         :img    (q/create-image scene/W scene/W :rgb)
         :paused false))

(defn- cell-of [^long px] (long (/ px scene/SCALE)))

(defn inject-mouse!
  "Drag to add smoke + a push force at the cursor."
  [fl]
  (when (q/mouse-pressed?)
    (let [n (long scene/N)
          i (cell-of (q/mouse-x))
          j (cell-of (q/mouse-y))]
      (when (and (>= i 0) (< i n) (>= j 0) (< j n))
        (let [^floats d  (:dens fl)
              ^floats fx (:fx fl)
              ^floats fy (:fy fl)
              dx (- (q/mouse-x) (q/pmouse-x))
              dy (- (q/mouse-y) (q/pmouse-y))]
          (doseq [oi (range -3 4) oj (range -3 4)]
            (let [ii (+ i oi) jj (+ j oj)]
              (when (and (>= ii 0) (< ii n) (>= jj 0) (< jj n))
                (let [k (f/idx n ii jj)]
                  (aset d  k (+ (aget d k) (float 0.5)))
                  (aset fx k (+ (aget fx k) (float (* 3.0 dx))))
                  (aset fy k (+ (aget fy k) (float (* 3.0 dy)))))))))))))

(defn update-state [state]
  (if (:paused state)
    state
    (let [p @params]
      (scene/seed-sources! state p)
      (inject-mouse! state)
      (let [s (scene/advance state p)]
        (reset! last-state s)
        s))))

(defn draw [state]
  (let [img ^PImage (:img state)]
    (.loadPixels img)
    (scene/render-pixels! state @params (.-pixels img))
    (.updatePixels img)
    (q/image img 0 0 (q/width) (q/height))))   ; scale render to fill the window/screen

(defn key-pressed [state event]
  (case (:key event)
    :r       (assoc (f/make-fluid scene/N) :img (:img state) :paused (:paused state))
    :space   (update state :paused not)
    state))

(defn start!
  "Launch (or relaunch) the sketch. Handlers are #'vars so REPL redefinition
   takes effect live. Calling again disposes the old window (resets the field).
   Pass :fullscreen true for present-mode fullscreen (ESC exits)."
  [& {:keys [fullscreen]}]
  (when-let [s @sketch] (try (.dispose ^processing.core.PApplet s) (catch Exception _)))
  (reset! sketch
          (if fullscreen
            (let [d (.getScreenSize (java.awt.Toolkit/getDefaultToolkit))]
              (q/sketch
               :title       "bjs-smoke-viz"
               :size        [(.width d) (.height d)]
               :features    [:present]
               :setup       #'setup
               :update      #'update-state
               :draw        #'draw
               :key-pressed #'key-pressed
               :middleware  [qm/fun-mode]))
            (q/sketch
             :title       "bjs-smoke-viz"
             :size        [scene/W scene/W]
             :setup       #'setup
             :update      #'update-state
             :draw        #'draw
             :key-pressed #'key-pressed
             :middleware  [qm/fun-mode]))))

;; ---- REPL dev helpers -----------------------------------------------------

(defn save-frame!
  "Save the last rendered frame to `path` (PNG) — reads the in-memory image."
  [path]
  (when-let [s @last-state]
    (.save ^PImage (:img s) path)
    path))

(defn dens-stats
  "Density-field readout for tuning: peak, total, #occupied cells."
  []
  (when-let [s @last-state]
    (let [^floats d (:dens s)
          mx  (areduce d i m 0.0 (max m (aget d i)))
          sum (areduce d i a 0.0 (+ a (aget d i)))
          nz  (areduce d i c 0 (if (> (aget d i) 0.01) (inc c) c))]
      {:max mx :sum sum :nonzero nz})))

(defn -main [& _] (start!))

(comment
  (swap! params assoc :dt 0.5)
  (swap! params assoc :dt 0.1)
  (swap! params assoc :visc 0.0001)
  (swap! params assoc :buoy 0.5)
  (swap! params assoc :emit 1)
  (swap! params assoc :keep 0.99)
  (swap! params assoc :edge-margin 10)
  (swap! params assoc :blur-passes 0)
  (swap! params assoc :expos 3.2))
