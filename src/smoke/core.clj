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
  (:import [processing.core PImage]
           [processing.opengl PGraphicsOpenGL])
  (:gen-class))

(def params (atom scene/default-params))
(defonce sketch     (atom nil))   ; running applet
(defonce last-state (atom nil))   ; latest sim state, for REPL inspection
(defonce reset?     (atom false)) ; set true to re-seed the field next frame (controls window)
(defonce pause-flip? (atom false)) ; set true to toggle pause next frame (controls window)
(defonce last-opts  (atom nil))   ; last start! options, so restart! relaunches at same size

(defn setup []
  (q/frame-rate 60)
  (q/background 0)              ; black from frame 1 (no gray default-canvas flash)
  (q/image-mode :corner)
  ;; smooth (bilinear) texture upscaling so the GPU doesn't show blocky pixels
  (try (.textureSampling ^PGraphicsOpenGL (q/current-graphics) 4) (catch Throwable _))
  (assoc (scene/new-fluid @params)
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
        (let [^floats dr (:dr fl) ^floats dg (:dg fl) ^floats db (:db fl)
              ^floats fx (:fx fl)
              ^floats fy (:fy fl)
              dx (- (q/mouse-x) (q/pmouse-x))
              dy (- (q/mouse-y) (q/pmouse-y))
              r  (long 8)          ; brush radius in cells (bigger = more smoke per frame)
              amp (float 0.9)]     ; peak density added at the centre
          (doseq [oi (range (- r) (inc r)) oj (range (- r) (inc r))]
            (let [ii (+ i oi) jj (+ j oj)
                  d2 (+ (* oi oi) (* oj oj))]
              (when (and (<= d2 (* r r)) (>= ii 0) (< ii n) (>= jj 0) (< jj n))
                (let [k (f/idx n ii jj)
                      ;; soft circular falloff: full at centre, fading to the rim
                      e (* amp (float (- 1.0 (/ (double d2) (double (* r r))))))]
                  ;; paint a bright dollop of smoke (white) + a push
                  (aset dr k (+ (aget dr k) e))
                  (aset dg k (+ (aget dg k) e))
                  (aset db k (+ (aget db k) e))
                  (aset fx k (+ (aget fx k) (float (* 3.0 dx))))
                  (aset fy k (+ (aget fy k) (float (* 3.0 dy)))))))))))))

(defn update-state [state]
  (let [state (if @pause-flip?
                (do (reset! pause-flip? false) (update state :paused not))
                state)]
    (cond
      @reset? (do (reset! reset? false)
                  (assoc (scene/new-fluid @params)
                         :img (:img state) :paused (:paused state)))
      (:paused state) state
      :else
      (let [p @params]
        (scene/seed-sources! state p)
        (inject-mouse! state)
        (let [s (scene/advance state p)]
          (reset! last-state s)
          s)))))

(defn draw [state]
  (let [img ^PImage (:img state)]
    (.loadPixels img)
    (scene/render-pixels! state @params (.-pixels img))
    (.updatePixels img)
    (q/image img 0 0 (q/width) (q/height))))   ; scale render to fill the window/screen

(defn- audio!
  "Call a smoke.audio fn by symbol if the ns is loaded (lazy => no ns cycle).
   Used to drive mpv playback from the sketch's transport keys."
  [sym & args]
  (when-let [f (try (requiring-resolve sym) (catch Throwable _))]
    (try (apply f args) (catch Throwable _))))

(defn key-pressed [state event]
  (case (:key event)
    :r       (do (audio! 'smoke.audio/restart-track!)   ; 'r' also restarts the audio from the top
                 (assoc (scene/new-fluid @params) :img (:img state) :paused (:paused state)))
    :space   (let [paused (not (:paused state))]
               (audio! 'smoke.audio/set-paused! paused)  ; space pauses/resumes audio with the sim
               (assoc state :paused paused))
    state))

(defn start!
  "Launch (or relaunch) the sketch on the OpenGL (:p2d) renderer, so the GPU
   scales the fixed-resolution (scene/W) render up to the window — big windows
   stay cheap. The CPU per-pixel render cost is fixed at scene/W regardless.

   Options:
     :fullscreen true  — present-mode fullscreen (ESC exits)
     :size  N          — windowed at N x N px (defaults to scene/W)
   Handlers are #'vars so REPL redefinition takes effect live; relaunching
   disposes the old window (and resets the field)."
  [& {:keys [fullscreen size] :as opts}]
  (reset! last-opts opts)   ; remembered so restart! can relaunch at the same size
  (when-let [s @sketch] (try (.dispose ^processing.core.PApplet s) (catch Exception _)))
  (let [dims (cond fullscreen (let [d (.getScreenSize (java.awt.Toolkit/getDefaultToolkit))]
                                [(.width d) (.height d)])
                   size       [size size]
                   :else      [scene/W scene/W])
        opts (concat
              [:title       "bjs-smoke-viz"
               :size        dims
               :renderer    :p2d
               :setup       #'setup
               :update      #'update-state
               :draw        #'draw
               :key-pressed #'key-pressed
               :middleware  [qm/fun-mode]]
              (when fullscreen [:features [:present]]))]
    (reset! sketch (apply q/sketch opts))
    ;; bring up the steering panel in a second window (lazy require => no cycle)
    (try ((requiring-resolve 'smoke.controls/open!)) (catch Throwable _))
    @sketch))

(defn restart!
  "Dispose the sketch window and relaunch it at the last-used size/fullscreen.
   Handy when the GL renderer freezes (run off the EDT, e.g. from a button)."
  []
  (apply start! (mapcat identity @last-opts)))

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

(defn -main [& _]
  (start!))

(comment

  (do
    (require '[smoke.core :reload true])
    (require '[smoke.scene :reload true])
    (start!))

  (swap! params assoc :dt 0.5)
  (swap! params assoc :dt 0.01)

  (swap! params assoc :dt 0.1)
  (swap! params assoc :visc 0.0001)
  (swap! params assoc :buoy 0.5)
  (swap! params assoc :emit 1)
  (swap! params assoc :keep 0.99)
  (swap! params assoc :edge-margin 10)
  (swap! params assoc :blur-passes 0)
  (swap! params assoc :expos 3.2))
