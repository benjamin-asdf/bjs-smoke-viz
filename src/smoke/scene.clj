(ns smoke.scene
  "Quil-free core: the simulation scene (sources + solver pipeline) and the
   density renderer. Shared by the live sketch (smoke.core) and the headless PNG
   renderer (smoke.headless), so the look is defined in exactly one place."
  (:require [smoke.fluid :as f]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const N     256)        ; n x n periodic grid (power of two => fast FFT)
(def ^:const SCALE 2)          ; window/image px per cell (=> 512x512)
(def ^:const W     (* N SCALE))

;; single source of truth for the tunable look (spectral solver)
(def default-params
  {:dt          0.25    ; sim speed — lower = slower smoke
   :visc        0.0003  ; spectral viscosity (exp(-|k|^2 dt visc)); higher = smoother
   :buoy        0.8     ; buoyancy (rise speed), force per unit density
   :emit        1.2     ; density added at the source each frame
   :lean        6.0     ; sideways force at the source (drift toward the window)
   :keep        0.97    ; density kept per frame (<1 => soft fade)
   :edge-margin 22      ; sponge-border width (cells); fades flow at edges (walls)
   :blur-passes 1       ; render-only density blur (residual speckle)
   :expos       3.2})   ; tonemap exposure: bulk gray, dense core white

(defn seed-blob!
  "Emit the steady smoke blob (lower-left) into the density field, and apply a
   rightward lean force at the blob so the plume drifts toward the window."
  [fl p]
  (let [n  (long N)
        ^floats d  (:dens fl)
        ^floats fx (:fx fl)
        emit (float (:emit p))
        lean (float (:lean p))
        cx (long (* 0.5 n)) cy (long (* 0.80 n)) r (long 7)]
    (dotimes [a (inc (* 2 r))]
      (let [oj (- a r) j (+ cy oj)]
        (dotimes [b (inc (* 2 r))]
          (let [oi (- b r) i (+ cx oi)]
            (when (and (<= (+ (* oi oi) (* oj oj)) (* r r))
                       (>= i 0) (< i n) (>= j 0) (< j n))
              (let [k (f/idx n i j)]
                (aset d  k (+ (aget d k) emit))
                (aset fx k (+ (aget fx k) lean))))))))))

(defn seed-sources!
  "Reset forces and emit this frame's source. Callers may add more (e.g. mouse)
   before calling `advance`."
  [fl p]
  (f/clear-forces! fl)
  (seed-blob! fl p)
  fl)

(defn advance
  "Run one solver tick with params `p` (sources already set this frame)."
  [fl p]
  (-> fl
      (f/vel-step (:visc p) (:dt p) (:buoy p))
      (f/advect-density (:dt p))
      (f/dissipate! (:keep p))
      (f/edge-fade! (:edge-margin p))))

(defn render-pixels!
  "Fill int-array `px` (length W*W, ARGB) with the tonemapped, blurred,
   bilinear-upscaled density. Used by both the Quil PImage and headless BufferedImage."
  [fl p ^ints px]
  (let [n      (long N)
        w      (long W)
        nm1    (dec n)
        ^floats dens (f/blur-density fl (:blur-passes p))
        gscale (/ (double nm1) (double (dec w)))
        expos  (double (:expos p))]
    (dotimes [oy w]
      (let [gy (* oy gscale) j0 (long gy) fy (- gy j0)
            j1 (min nm1 (inc j0)) row (* oy w)]
        (dotimes [ox w]
          (let [gx (* ox gscale) i0 (long gx) fx (- gx i0)
                i1 (min nm1 (inc i0))
                d00 (aget dens (f/idx n i0 j0)) d10 (aget dens (f/idx n i1 j0))
                d01 (aget dens (f/idx n i0 j1)) d11 (aget dens (f/idx n i1 j1))
                d   (+ (* (- 1.0 fy) (+ (* (- 1.0 fx) d00) (* fx d10)))
                       (* fy        (+ (* (- 1.0 fx) d01) (* fx d11))))
                t   (- 1.0 (Math/exp (- (* expos d))))
                c   (long (* 255.0 (max 0.0 (min 1.0 t))))]
            (aset px (+ ox row)
                  (unchecked-int (bit-or (unchecked-int 0xFF000000)
                                         (bit-shift-left c 16) (bit-shift-left c 8) c)))))))
    px))
