(ns smoke.fluid
  "Spectral 2D smoke solver — Jos Stam's original *Stable Fluids* (SIGGRAPH 1999),
   the FFT formulation. Per frame:

     add force -> self-advect (semi-Lagrangian) -> FFT -> diffuse+project -> IFFT

   The clever step is diffuse+project, done EXACTLY in Fourier space in one pass.
   For each wavevector k=(kx,ky):
     f  = exp(-|k|^2 * dt * visc)                       ; viscosity damping
     U' = f * [(1 - kx^2/|k|^2) U - (kx ky/|k|^2) V]    ; remove the component of
     V' = f * [(- kx ky/|k|^2) U + (1 - ky^2/|k|^2) V]  ; velocity parallel to k

   Removing the k-parallel component IS the projection onto divergence-free
   fields — exact, non-iterative, free of the collocated checkerboard. The FFT
   domain is PERIODIC, so the grid is a plain n x n (no boundary ring).

   Density is carried in three colour channels (:dr :dg :db) so multiple coloured
   sources mix additively (red + blue overlap => magenta). All channels advect on
   the shared velocity field. :dens holds their sum, used for buoyancy/wind."
  (:import [org.jtransforms.fft FloatFFT_2D]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn idx ^long [^long n ^long i ^long j]
  (unchecked-add i (unchecked-multiply n j)))

;; ---- row-band parallelism --------------------------------------------------
;; The hot loops here and in smoke.scene are all row-disjoint (each output row is
;; written by exactly one iteration), so they parallelise trivially. dtype-next's
;; parallel-for gave no speed-up on these (its batching serialised them), but a
;; plain split into ~cpu row-bands on the future pool gives ~7x. `par-rows` is
;; that split; small ranges fall back to serial (thread overhead not worth it).
(def ^:const ROWS-PER-BAND 24)            ; min rows per worker => no over-splitting tiny grids
(def ^:private NCPU (.availableProcessors (Runtime/getRuntime)))

(defn par-rows
  "Call `(rowfn i)` for i in [0,n). Splits the range into up to NCPU contiguous
   bands run on the future pool; serial below 2 bands. `rowfn` should take a
   primitive-long row index and do that whole row's work."
  [^long n rowfn]
  (let [nt (min (long NCPU) (quot n (long ROWS-PER-BAND)))]
    (if (<= nt 1)
      (dotimes [i n] (rowfn i))
      (let [band (quot n nt)]
        (->> (range nt)
             (mapv (fn [^long t]
                     (let [lo (* t band) hi (if (= t (dec nt)) n (* (inc t) band))]
                       (future (loop [i lo] (when (< i hi) (rowfn i) (recur (inc i))))))))
             (run! deref))))))

(defn make-fluid
  "Allocate fields + the FFT plan for an n x n periodic grid."
  [^long n]
  (let [sz (* n n) f #(float-array sz)]
    {:n    n
     :u    (f) :v    (f)              ; velocity
     :u0   (f) :v0   (f)              ; advect scratch (previous velocity)
     :fx   (f) :fy   (f)              ; force accumulators (set each frame)
     :cu   (float-array (* 2 sz))     ; complex velocity buffers for the FFT
     :cv   (float-array (* 2 sz))
     :dens (f)                        ; total density (sum of channels), for physics
     :dr   (f) :dg   (f) :db   (f)    ; colour density channels
     :dr0  (f) :dg0  (f) :db0  (f)    ; channel advect scratch
     :br   (f) :bg   (f) :bb   (f)    ; render-blur output per channel
     :tmp  (f)                        ; blur ping-pong scratch
     :fft  (FloatFFT_2D. n n)}))

(defn clear-forces!
  "Zero the per-frame force fields."
  [f]
  (java.util.Arrays/fill ^floats (:fx f) (float 0.0))
  (java.util.Arrays/fill ^floats (:fy f) (float 0.0))
  f)

(defn- interp
  "Bilinear sample of field `q` at (x,y) in cell units, with periodic wrap."
  ^double [^floats q ^long n ^double x ^double y]
  (let [x  (let [m (rem x n)] (if (neg? m) (+ m n) m))
        y  (let [m (rem y n)] (if (neg? m) (+ m n) m))
        i0 (long x) j0 (long y)
        i1 (let [v (inc i0)] (if (>= v n) 0 v))
        j1 (let [v (inc j0)] (if (>= v n) 0 v))
        sx (- x i0) sy (- y j0)
        q00 (aget q (idx n i0 j0)) q10 (aget q (idx n i1 j0))
        q01 (aget q (idx n i0 j1)) q11 (aget q (idx n i1 j1))]
    (+ (* (- 1.0 sy) (+ (* (- 1.0 sx) q00) (* sx q10)))
       (* sy        (+ (* (- 1.0 sx) q01) (* sx q11))))))

(defn- advect!
  "Semi-Lagrangian advect of `dst` (sampling `src`) along velocity (vu,vv)."
  [n ^floats dst ^floats src ^floats vu ^floats vv dt]
  (let [n (long n) dt (double dt)]
    (par-rows
     n
     (fn [^long j]
       (dotimes [i n]
         (let [k (idx n i j)
               x (- i (* dt (aget vu k)))
               y (- j (* dt (aget vv k)))]
           (aset dst k (float (interp src n x y)))))))))

(defn vel-step
  "Advance velocity one tick: buoyancy -> add forces -> self-advect -> FFT ->
   diffuse+project -> IFFT. :fx/:fy hold this frame's external forces; buoyancy
   (proportional to total density, upward = -y) is added here."
  [f visc dt buoy]
  (let [n (long (:n f)) sz (* n n)
        visc (double visc) dt (double dt) buoy (double buoy)
        ^floats u (:u f) ^floats v (:v f)
        ^floats u0 (:u0 f) ^floats v0 (:v0 f)
        ^floats fx (:fx f) ^floats fy (:fy f) ^floats dens (:dens f)
        ^floats cu (:cu f) ^floats cv (:cv f)
        ^FloatFFT_2D fft (:fft f)
        half (quot n 2)]
    (par-rows
     n
     (fn [^long j]
       (dotimes [i n]
         (let [k (idx n i j)]
           (aset u k (float (+ (aget u k) (* dt (aget fx k)))))
           (aset v k (float (+ (aget v k) (* dt (- (aget fy k) (* buoy (aget dens k)))))))))))
    (System/arraycopy u 0 u0 0 sz)
    (System/arraycopy v 0 v0 0 sz)
    (advect! n u u0 u0 v0 dt)
    (advect! n v v0 u0 v0 dt)
    (par-rows
     n
     (fn [^long j]
       (dotimes [i n]
         (let [k (idx n i j) b (* 2 k)]
           (aset cu b (aget u k)) (aset cu (inc b) (float 0.0))
           (aset cv b (aget v k)) (aset cv (inc b) (float 0.0))))))
    (.complexForward fft cu)
    (.complexForward fft cv)
    (par-rows
     n
     (fn [^long j]
       (let [ky (double (if (<= j half) j (- j n)))]
         (dotimes [i n]
           (let [kx (double (if (<= i half) i (- i n)))
                 kk (+ (* kx kx) (* ky ky))]
             (when (>= kk 1.0e-3)
               (let [b   (* 2 (idx n i j))
                     fdamp (Math/exp (- (* kk dt visc)))
                     a   (- 1.0 (/ (* kx kx) kk))
                     bb  (/ (* kx ky) kk)
                     c   (- 1.0 (/ (* ky ky) kk))
                     ure (aget cu b) uim (aget cu (inc b))
                     vre (aget cv b) vim (aget cv (inc b))]
                 (aset cu b       (float (* fdamp (- (* a ure) (* bb vre)))))
                 (aset cu (inc b) (float (* fdamp (- (* a uim) (* bb vim)))))
                 (aset cv b       (float (* fdamp (+ (* (- bb) ure) (* c vre)))))
                 (aset cv (inc b) (float (* fdamp (+ (* (- bb) uim) (* c vim))))))))))))
    (.complexInverse fft cu true)
    (.complexInverse fft cv true)
    (par-rows
     n
     (fn [^long j]
       (dotimes [i n]
         (let [k (idx n i j)]
           (aset u k (aget cu (* 2 k)))
           (aset v k (aget cv (* 2 k)))))))
    f))

(defn compute-total!
  "Sum the colour channels into :dens (used by buoyancy and wind)."
  [f]
  (let [n (long (:n f))
        ^floats dens (:dens f) ^floats dr (:dr f) ^floats dg (:dg f) ^floats db (:db f)]
    (par-rows
     n
     (fn [^long j]
       (dotimes [i n]
         (let [k (idx n i j)]
           (aset dens k (+ (aget dr k) (aget dg k) (aget db k)))))))
    f))

(defn advect-colors!
  "Advect all three colour channels along the velocity field."
  [f dt]
  (let [n (long (:n f)) sz (* n n) dt (double dt)
        ^floats u (:u f) ^floats v (:v f)
        ^floats dr (:dr f) ^floats dg (:dg f) ^floats db (:db f)
        ^floats dr0 (:dr0 f) ^floats dg0 (:dg0 f) ^floats db0 (:db0 f)]
    (System/arraycopy dr 0 dr0 0 sz) (advect! n dr dr0 u v dt)
    (System/arraycopy dg 0 dg0 0 sz) (advect! n dg dg0 u v dt)
    (System/arraycopy db 0 db0 0 sz) (advect! n db db0 u v dt)
    f))

(defn dissipate-colors!
  "Fade every colour channel by `keep` per tick."
  [f keep]
  (let [n (long (:n f)) k (float keep)
        ^floats dr (:dr f) ^floats dg (:dg f) ^floats db (:db f)]
    (par-rows
     n
     (fn [^long j]
       (dotimes [i n]
         (let [ix (idx n i j)]
           (aset dr ix (* (aget dr ix) k))
           (aset dg ix (* (aget dg ix) k))
           (aset db ix (* (aget db ix) k)))))))
  f)

(defn dissipate-colors-rgb!
  "Fade each colour channel by its own keep factor (audio-reactive layer)."
  [f kr kg kb]
  (let [n (long (:n f)) kr (float kr) kg (float kg) kb (float kb)
        ^floats dr (:dr f) ^floats dg (:dg f) ^floats db (:db f)]
    (par-rows
     n
     (fn [^long j]
       (dotimes [i n]
         (let [ix (idx n i j)]
           (aset dr ix (* (aget dr ix) kr))
           (aset dg ix (* (aget dg ix) kg))
           (aset db ix (* (aget db ix) kb)))))))
  f)

(defn edge-fade!
  "Absorbing sponge border: fade colour channels AND velocity to 0 within
   `margin` cells of any edge, so the periodic domain behaves like walls."
  [f margin]
  (let [n (long (:n f)) m (double margin)
        ^floats dr (:dr f) ^floats dg (:dg f) ^floats db (:db f)
        ^floats u (:u f) ^floats v (:v f)]
    (when (pos? m)
      (par-rows
       n
       (fn [^long j]
         (let [dj (min j (- n 1 j))]
           (dotimes [i n]
             (let [dist (double (min dj (min i (- n 1 i))))]
               (when (< dist m)
                 (let [k (idx n i j) fac (float (/ dist m))]
                   (aset dr k (* (aget dr k) fac))
                   (aset dg k (* (aget dg k) fac))
                   (aset db k (* (aget db k) fac))
                   (aset u k (* (aget u k) fac))
                   (aset v k (* (aget v k) fac))))))))))
    f))

(defn- blur-into
  "Separable box blur of `src` into `out` (using `tmp` as ping-pong), `passes`."
  [n ^floats src ^floats out ^floats tmp passes]
  (let [n (long n) p (long passes) third (/ 1.0 3.0)]
    (System/arraycopy src 0 out 0 (* n n))
    (dotimes [_ p]
      (par-rows
       n
       (fn [^long j]
         (dotimes [i n]
           (let [il (if (zero? i) (dec n) (dec i))
                 ir (if (= i (dec n)) 0 (inc i))]
             (aset tmp (idx n i j)
                   (float (* third (+ (aget out (idx n il j)) (aget out (idx n i j))
                                      (aget out (idx n ir j))))))))))
      (par-rows
       n
       (fn [^long j]
         (dotimes [i n]
           (let [jd (if (zero? j) (dec n) (dec j))
                 ju (if (= j (dec n)) 0 (inc j))]
             (aset out (idx n i j)
                   (float (* third (+ (aget tmp (idx n i jd)) (aget tmp (idx n i j))
                                      (aget tmp (idx n i ju)))))))))))))

(defn blur-colors!
  "Blur each colour channel into :br/:bg/:bb for rendering (sim untouched)."
  [f passes]
  (let [n (long (:n f)) tmp (:tmp f)]
    (blur-into n (:dr f) (:br f) tmp passes)
    (blur-into n (:dg f) (:bg f) tmp passes)
    (blur-into n (:db f) (:bb f) tmp passes)
    f))
