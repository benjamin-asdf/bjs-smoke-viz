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
   (mass-conserving) fields — exact, non-iterative, and free of the collocated
   checkerboard that plagued the Gauss-Seidel version. No boundary ring either:
   the FFT domain is PERIODIC, so the grid is a plain n x n and smoke wraps
   edge-to-edge.

   Fields are flat `float-array`s, POS(i,j) = i + n*j. Forces (:fx/:fy) are set
   by the caller each frame; velocity self-advects, density advects on velocity."
  (:import [org.jtransforms.fft FloatFFT_2D]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn idx ^long [^long n ^long i ^long j]
  (unchecked-add i (unchecked-multiply n j)))

(defn make-fluid
  "Allocate fields + the FFT plan for an n x n periodic grid."
  [^long n]
  (let [sz (* n n) f #(float-array sz)]
    {:n    n
     :u    (f) :v    (f)              ; velocity
     :u0   (f) :v0   (f)              ; advect scratch (previous velocity)
     :dens (f) :dens0 (f)             ; density + advect scratch
     :fx   (f) :fy   (f)              ; force accumulators (set each frame)
     :cu   (float-array (* 2 sz))     ; complex velocity buffers for the FFT
     :cv   (float-array (* 2 sz))
     :blur (f) :tmp (f)               ; render-blur scratch
     :fft  (FloatFFT_2D. n n)}))

(defn clear-forces!
  "Zero the per-frame force fields. Call at the top of a frame, then add forces
   (buoyancy is applied inside vel-step; the caller adds any others into :fx/:fy)."
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
    (dotimes [j n]
      (dotimes [i n]
        (let [k (idx n i j)
              x (- i (* dt (aget vu k)))
              y (- j (* dt (aget vv k)))]
          (aset dst k (float (interp src n x y))))))))

(defn vel-step
  "Advance velocity one tick: buoyancy -> add forces -> self-advect -> FFT ->
   diffuse+project -> IFFT. :fx/:fy hold this frame's external forces; buoyancy
   (proportional to density, upward = -y) is added here."
  [f visc dt buoy]
  (let [n (long (:n f)) sz (* n n)
        visc (double visc) dt (double dt) buoy (double buoy)
        ^floats u (:u f) ^floats v (:v f)
        ^floats u0 (:u0 f) ^floats v0 (:v0 f)
        ^floats fx (:fx f) ^floats fy (:fy f) ^floats dens (:dens f)
        ^floats cu (:cu f) ^floats cv (:cv f)
        ^FloatFFT_2D fft (:fft f)
        half (quot n 2)]
    ;; forces: buoyancy (up) + caller's :fx/:fy
    (dotimes [k sz]
      (aset u k (float (+ (aget u k) (* dt (aget fx k)))))
      (aset v k (float (+ (aget v k) (* dt (- (aget fy k) (* buoy (aget dens k))))))))
    ;; self-advection
    (System/arraycopy u 0 u0 0 sz)
    (System/arraycopy v 0 v0 0 sz)
    (advect! n u u0 u0 v0 dt)
    (advect! n v v0 u0 v0 dt)
    ;; load real velocity into interleaved complex buffers (imag = 0)
    (dotimes [k sz]
      (let [b (* 2 k)]
        (aset cu b (aget u k))       (aset cu (inc b) (float 0.0))
        (aset cv b (aget v k))       (aset cv (inc b) (float 0.0))))
    (.complexForward fft cu)
    (.complexForward fft cv)
    ;; diffuse + project, per wavevector
    (dotimes [j n]
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
                (aset cv (inc b) (float (* fdamp (+ (* (- bb) uim) (* c vim)))))))))))
    (.complexInverse fft cu true)   ; true => normalize by 1/(n*n)
    (.complexInverse fft cv true)
    (dotimes [k sz]
      (aset u k (aget cu (* 2 k)))
      (aset v k (aget cv (* 2 k))))
    f))

(defn advect-density
  "Advance density one tick by advecting it along the velocity field."
  [f dt]
  (let [n (long (:n f)) sz (* n n)
        ^floats dens (:dens f) ^floats d0 (:dens0 f)
        ^floats u (:u f) ^floats v (:v f)]
    (System/arraycopy dens 0 d0 0 sz)
    (advect! n dens d0 u v (double dt))
    f))

(defn dissipate!
  "Fade density by `keep` per tick (<1 => soft fade, no pile-up)."
  [f keep]
  (let [^floats d (:dens f) k (float keep)]
    (dotimes [i (alength d)] (aset d i (* (aget d i) k))))
  f)

(defn edge-fade!
  "Absorbing sponge border: fade density AND velocity to 0 within `margin` cells
   of any edge. On the periodic FFT domain this makes the borders behave like
   containing walls — flow dies at the edge instead of wrapping around. margin<=0
   disables."
  [f margin]
  (let [n (long (:n f)) m (double margin)
        ^floats d (:dens f) ^floats u (:u f) ^floats v (:v f)]
    (when (pos? m)
      (dotimes [j n]
        (let [dj (min j (- n 1 j))]
          (dotimes [i n]
            (let [dist (double (min dj (min i (- n 1 i))))]
              (when (< dist m)
                (let [k (idx n i j) fac (float (/ dist m))]
                  (aset d k (* (aget d k) fac))
                  (aset u k (* (aget u k) fac))
                  (aset v k (* (aget v k) fac))))))))))
  f)

(defn blur-density
  "Separable box blur of density into :blur (sim untouched), `passes` strength.
   Returns the blurred array for the renderer — removes residual speckle.
   Periodic, matching the solver domain."
  [f passes]
  (let [n (long (:n f)) p (long passes) third (/ 1.0 3.0)
        ^floats a (:blur f) ^floats b (:tmp f) ^floats dens (:dens f)]
    (System/arraycopy dens 0 a 0 (alength dens))
    (dotimes [_ p]
      (dotimes [j n]
        (dotimes [i n]
          (let [il (if (zero? i) (dec n) (dec i))
                ir (if (= i (dec n)) 0 (inc i))]
            (aset b (idx n i j)
                  (float (* third (+ (aget a (idx n il j)) (aget a (idx n i j))
                                     (aget a (idx n ir j)))))))))
      (dotimes [j n]
        (dotimes [i n]
          (let [jd (if (zero? j) (dec n) (dec j))
                ju (if (= j (dec n)) 0 (inc j))]
            (aset a (idx n i j)
                  (float (* third (+ (aget b (idx n i jd)) (aget b (idx n i j))
                                     (aget b (idx n i ju))))))))))
    a))
