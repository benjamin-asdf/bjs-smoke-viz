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
   Interact: drag mouse to push smoke. space = pause, r = reset,
   ←/→ = seek ±5s, ↓/↑ = seek ±60s (all also drive mpv audio playback)."
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
  (let [p @params]
    (assoc (scene/new-fluid p)
           :img    (q/create-image (scene/render-w p) (scene/render-h p) :rgb)
           :paused false)))

(defn inject-mouse!
  "Drag to add smoke + a push force at the cursor."
  [fl]
  (when (q/mouse-pressed?)
    (let [p @params
          n (long (:n fl))
          ;; window px -> grid cell, accounting for the render's cover-crop mapping
          [i j] (scene/win->cell p (q/width) (q/height) (q/mouse-x) (q/mouse-y))
          i (long i) j (long j)]
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

(defn- audio!
  "Call a smoke.audio fn by symbol if the ns is loaded (lazy => no ns cycle).
   Used to drive mpv playback (pause/seek/restart) from the sketch."
  [sym & args]
  (when-let [f (try (requiring-resolve sym) (catch Throwable _))]
    (try (apply f args) (catch Throwable _))))

(defn update-state [state]
  (let [state (if @pause-flip?
                ;; single pause toggle (space key + controls button both set the
                ;; flag): flip :paused AND pause/resume the mpv audio in lockstep
                (do (reset! pause-flip? false)
                    (let [paused (not (:paused state))]
                      (audio! 'smoke.audio/set-paused! paused)
                      (assoc state :paused paused)))
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
  (let [img ^PImage (:img state)
        p   @params
        w   (q/width) h (q/height)]
    (.loadPixels img)
    (scene/render-pixels! state p (.-pixels img))
    (.updatePixels img)
    (if (:depth-layer p)
      ;; additive zoom-tunnel on black: nested copies, each larger (edges off-
      ;; screen => no visible square) and dimmer, drawn back-to-front so the
      ;; pattern appears to recede into depth behind the full-size front.
      (let [n    (long (:depth-layers p 3))
            step (double (:depth-scale p 0.35))
            dim  (double (:depth-dim p 0.5))]
        (q/background 0)
        (q/blend-mode :add)
        (doseq [i (range n 0 -1)]
          (let [s  (+ 1.0 (* i step))
                b  (int (* 255.0 (Math/pow dim i)))
                bw (* w s) bh (* h s)
                ox (* 0.5 (- w bw)) oy (* 0.5 (- h bh))]
            (q/tint b b b)
            (q/image img ox oy bw bh)))
        (q/no-tint)
        (q/image img 0 0 w h)              ; full front layer
        (q/blend-mode :blend))
      (q/image img 0 0 w h))))             ; scale render to fill the window/screen

(defn key-pressed [state event]
  (case (:key event)
    :r       (do (audio! 'smoke.audio/restart-track!)   ; 'r' also restarts the audio from the top
                 (assoc (scene/new-fluid @params) :img (:img state) :paused (:paused state)))
    ;; route pause through pause-flip? so the key and the controls-window button
    ;; share ONE toggle path (update-state) => audio always follows the sim
    :space   (do (reset! pause-flip? true) state)
    ;; arrows scrub the audio (and its modulation clock) — mpv-style steps
    :left    (do (audio! 'smoke.audio/seek!  -5.0) state)
    :right   (do (audio! 'smoke.audio/seek!   5.0) state)
    :down    (do (audio! 'smoke.audio/seek! -60.0) state)
    :up      (do (audio! 'smoke.audio/seek!  60.0) state)
    state))

(defn start!
  "Launch (or relaunch) the sketch on the OpenGL (:p2d) renderer, so the GPU
   scales the internal render (scene/render-w x scene/render-h) up to the window.
   The window can be any size — the CPU per-pixel cost is fixed by the RENDER
   resolution, not the window. Bump the render resolution for a crisper big frame.

   Options:
     :fullscreen true  — present-mode fullscreen (ESC exits)
     :size  N          — square window N x N px
     :size  [w h]      — window w x h px (defaults to the render resolution)
     :render [rw rh]   — internal render resolution (=> params :render-w/:render-h);
                         e.g. [1920 1080] for a crisp 1080p frame
     :grid  N          — sim grid size (=> params :grid-n); higher = finer smoke
   Handlers are #'vars so REPL redefinition takes effect live; relaunching
   disposes the old window (and resets the field)."
  [& {:keys [fullscreen size render grid] :as opts}]
  (reset! last-opts opts)   ; remembered so restart! can relaunch at the same size
  (when render (swap! params assoc :render-w (first render) :render-h (second render)))
  (when grid   (swap! params assoc :grid-n grid))
  (when-let [s @sketch] (try (.dispose ^processing.core.PApplet s) (catch Exception _)))
  (let [p @params
        dims (cond fullscreen (let [d (.getScreenSize (java.awt.Toolkit/getDefaultToolkit))]
                                [(.width d) (.height d)])
                   (vector? size) size
                   size       [size size]
                   :else      [(scene/render-w p) (scene/render-h p)])
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
