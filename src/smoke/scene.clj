(ns smoke.scene
  "Quil-free core: the scene (moving coloured sources + noise-wind + solver
   pipeline) and the RGB renderer. Shared by the live sketch (smoke.core) and the
   headless PNG renderer (smoke.headless).

   A `theme` is a whole scene preset: a list of sources, each with its own colour,
   emit rate, radius, and motion. Density is carried in three colour channels, so
   overlapping jets mix additively (red + blue => magenta). The wind is a
   procedural noise flow-field, so the smoke gusts and swirls instead of drifting
   one fixed way."
  (:require [smoke.fluid :as f]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const N     256)        ; n x n periodic grid; bump to 512 for finer detail (4x FFT cost)
(def ^:const SCALE 3)          ; window/image px per cell (=> 768x768). Raise for a bigger window.
(def ^:const W     (* N SCALE))
(def ^:const TAU   6.283185307179586)

;; ---- themes = scene presets (lists of moving coloured sources) -------------
;; source: {:color [r g b] (0..1) :emit rate :r radius-cells
;;          :motion {:type :static|:osc-x|:osc-y|:circle :base [fx fy] :amp :speed :phase}}
(def themes
  {:rgb    [{:color [1.0 0.15 0.10] :emit 1.0 :r 3 :motion {:type :brownian :base [0.35 0.55] :amp 0.010}}
            {:color [0.10 0.45 1.0] :emit 1.0 :r 3 :motion {:type :brownian :base [0.65 0.55] :amp 0.010}}
            {:color [0.20 1.0 0.30] :emit 0.8 :r 2 :motion {:type :brownian :base [0.50 0.50] :amp 0.012}}]
   :duet   [{:color [1.0 0.20 0.60] :emit 1.0 :r 3 :motion {:type :osc-x  :base [0.40 0.82] :amp 0.22 :speed 0.030}}
            {:color [0.20 0.85 1.0] :emit 1.0 :r 3 :motion {:type :osc-x  :base [0.60 0.82] :amp 0.22 :speed 0.030 :phase 3.14159}}]
   :single [{:color [1.0 0.55 0.20] :emit 1.0 :r 3 :motion {:type :static :base [0.50 0.82]}}]})

(def default-params
  {:dt          0.05     ; sim speed — lower = slower smoke
   :visc        0.0001   ; spectral viscosity (exp(-|k|^2 dt visc)); higher = smoother
   :buoy        0.5      ; buoyancy (rise speed), force per unit total density
   :keep        0.99     ; density kept per frame (<1 => soft fade)
   :edge-margin 10       ; sponge-border width (cells); fades flow at edges (walls)
   :blur-passes 1        ; render-only density blur (residual speckle)
   :expos       2.4      ; tonemap exposure per colour channel
   :wind        5.0      ; wind strength (noise flow-field force on the smoke)
   :noise-scale 2.0      ; wind spatial frequency (cells of swirl)
   :noise-speed 0.02     ; how fast the wind field evolves
   :theme       :rgb})   ; scene preset — one of (keys themes)

;; ---- moving coloured sources ----------------------------------------------
(defonce frame   (atom 0))    ; frame counter, drives deterministic motion + wind time
(defonce src-pos (atom nil))  ; {:theme kw :pos [[x y] ...]} — live source positions

(defn- clamp01m [^double x] (min 0.9 (max 0.1 x)))

(defn- next-pos
  "Next position for a source: brownian random-walks from its previous position;
   deterministic motions are computed from base + time."
  [src prev ^double t]
  (let [m (:motion src)]
    (if (= (:type m) :brownian)
      (let [step (double (:amp m 0.01))]
        [(clamp01m (+ (double (first prev))  (* step 2.0 (- (double (rand)) 0.5))))
         (clamp01m (+ (double (second prev)) (* step 2.0 (- (double (rand)) 0.5))))])
      (let [base (:base m)
            bx (double (first base)) by (double (second base))
            amp (double (:amp m 0.0)) speed (double (:speed m 0.0))
            phase (double (:phase m 0.0))
            ph (+ (* speed t) phase)]
        (case (:type m)
          :osc-x  [(+ bx (* amp (Math/sin ph))) by]
          :osc-y  [bx (+ by (* amp (Math/sin ph)))]
          :circle [(+ bx (* amp (Math/cos ph))) (+ by (* amp (Math/sin ph)))]
          [bx by])))))

(defn- emit-sources! [fl srcs positions]
  (let [n (long N)
        ^floats dr (:dr fl) ^floats dg (:dg fl) ^floats db (:db fl)]
    (doseq [[src pos] (map vector srcs positions)]
      (let [color (:color src)
            e  (double (:emit src))
            er (float (* e (double (nth color 0))))
            eg (float (* e (double (nth color 1))))
            eb (float (* e (double (nth color 2))))
            cx (long (* (double (first pos)) n))
            cy (long (* (double (second pos)) n))
            r  (long (:r src))]
        (dotimes [a (inc (* 2 r))]
          (let [oj (- a r) jj (+ cy oj)]
            (dotimes [b (inc (* 2 r))]
              (let [oi (- b r) ii (+ cx oi)]
                (when (and (<= (+ (* oi oi) (* oj oj)) (* r r))
                           (>= ii 0) (< ii n) (>= jj 0) (< jj n))
                  (let [k (f/idx n ii jj)]
                    (aset dr k (+ (aget dr k) er))
                    (aset dg k (+ (aget dg k) eg))
                    (aset db k (+ (aget db k) eb))))))))))))

;; ---- noise flow-field wind -------------------------------------------------
(defn- apply-wind! [fl p ^double t]
  (let [n (long N)
        ^floats fx (:fx fl) ^floats fy (:fy fl) ^floats d (:dens fl)
        w   (double (:wind p))
        sc  (* TAU (double (:noise-scale p)))
        sp  (* (double (:noise-speed p)) t)
        inv (/ 1.0 (double n))]
    (when (pos? w)
      (dotimes [j n]
        (let [yn (* j inv)]
          (dotimes [i n]
            (let [k (f/idx n i j) dk (aget d k)]
              (when (> dk 1.0e-4)
                (let [xn (* i inv)
                      wx (+ (Math/sin (+ (* sc yn) sp))
                            (* 0.5 (Math/sin (- (* sc 1.7 yn) (* sc 0.9 xn) (* 1.3 sp)))))
                      wy (+ (Math/cos (- (* sc xn) sp))
                            (* 0.5 (Math/sin (+ (* sc 1.3 xn) (* sc 1.1 yn) (* 0.7 sp)))))]
                  (aset fx k (float (+ (aget fx k) (* w wx dk))))
                  (aset fy k (float (+ (aget fy k) (* w wy dk)))))))))))))

(defn seed-sources!
  "Reset forces, advance time, emit all of the theme's moving sources, then apply
   the noise wind. Callers may add more (e.g. mouse) before `advance`."
  [fl p]
  (f/clear-forces! fl)
  (let [t    (double (swap! frame inc))
        srcs (get themes (:theme p) (:single themes))]
    ;; (re)initialise live positions when the theme changes
    (when (not= (:theme p) (:theme @src-pos))
      (reset! src-pos {:theme (:theme p) :pos (mapv :base (map :motion srcs))}))
    (let [positions (mapv #(next-pos %1 %2 t) srcs (:pos @src-pos))]
      (swap! src-pos assoc :pos positions)
      (emit-sources! fl srcs positions))
    (f/compute-total! fl)
    (apply-wind! fl p t))
  fl)

(defn advance
  "Run one solver tick with params `p` (sources already set this frame)."
  [fl p]
  (-> fl
      (f/vel-step (:visc p) (:dt p) (:buoy p))
      (f/advect-colors! (:dt p))
      (f/dissipate-colors! (:keep p))
      (f/edge-fade! (:edge-margin p))))

;; ---- render ----------------------------------------------------------------
(def ^:const TLUTN 512)
(def ^:const CHMAX 3.0)   ; density per channel that maps to ~full brightness

(defn- tone-lut ^floats [expos]
  (let [lut (float-array TLUTN) e (double expos)]
    (dotimes [i TLUTN]
      (let [ch (* (/ (double i) (dec TLUTN)) CHMAX)]
        (aset lut i (float (* 255.0 (- 1.0 (Math/exp (- (* e ch)))))))))
    lut))

(defn render-pixels!
  "Fill int-array `px` (length W*W, ARGB) by compositing the three blurred,
   bilinear-upscaled, tonemapped colour channels."
  [fl p ^ints px]
  (f/blur-colors! fl (:blur-passes p))
  (let [n      (long N)
        w      (long W)
        nm1    (dec n)
        ^floats br (:br fl) ^floats bg (:bg fl) ^floats bb (:bb fl)
        ^floats tl (tone-lut (:expos p))
        tscale (/ (double (dec TLUTN)) CHMAX)
        gscale (/ (double nm1) (double (dec w)))]
    (dotimes [oy w]
      (let [gy (* oy gscale) j0 (long gy) fy (- gy j0)
            j1 (min nm1 (inc j0)) sy0 (- 1.0 fy) row (* oy w)]
        (dotimes [ox w]
          (let [gx (* ox gscale) i0 (long gx) fx (- gx i0)
                i1 (min nm1 (inc i0)) sx0 (- 1.0 fx)
                k00 (f/idx n i0 j0) k10 (f/idx n i1 j0)
                k01 (f/idx n i0 j1) k11 (f/idx n i1 j1)
                cr (+ (* sy0 (+ (* sx0 (aget br k00)) (* fx (aget br k10))))
                      (* fy  (+ (* sx0 (aget br k01)) (* fx (aget br k11)))))
                cg (+ (* sy0 (+ (* sx0 (aget bg k00)) (* fx (aget bg k10))))
                      (* fy  (+ (* sx0 (aget bg k01)) (* fx (aget bg k11)))))
                cb (+ (* sy0 (+ (* sx0 (aget bb k00)) (* fx (aget bb k10))))
                      (* fy  (+ (* sx0 (aget bb k01)) (* fx (aget bb k11)))))
                ri (long (aget tl (min (dec TLUTN) (max 0 (long (* cr tscale))))))
                gi (long (aget tl (min (dec TLUTN) (max 0 (long (* cg tscale))))))
                bi (long (aget tl (min (dec TLUTN) (max 0 (long (* cb tscale))))))]
            (aset px (+ ox row)
                  (unchecked-int (bit-or (unchecked-int 0xFF000000)
                                         (bit-shift-left ri 16) (bit-shift-left gi 8) bi)))))))
    px))
