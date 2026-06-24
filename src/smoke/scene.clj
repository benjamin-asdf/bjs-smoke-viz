(ns smoke.scene
  "Quil-free core: the scene + RGB renderer, shared by the live sketch
   (smoke.core) and the headless renderer (smoke.headless).

   A `theme` bundles a MODE with its config; switch theme = switch the whole look:

     :jets    — colored moving SOURCES emit the smoke (Brownian + boids flocking),
                no Physarum. (Default — the look at startup.)
     :slime   — Physarum agents emit COLOURED smoke: they sense the smoke density
                (density == trail) and deposit their colour; the fluid advects it.
     :network — classic WHITE Physarum network on a separate trail map.

   The spectral fluid solver advects/dissipates the smoke; a procedural noise
   wind drags everything."
  (:require [smoke.fluid :as f]
            [smoke.physarum :as phys]
            [smoke.boids :as boids]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const N     256)        ; n x n periodic grid; bump to 512 for finer detail (4x FFT cost)
(def ^:const SCALE 3)          ; window/image px per cell (=> 768x768). Bigger costs CPU quadratically.
(def ^:const W     (* N SCALE))
(def ^:const TAU   6.283185307179586)

;; ---- themes = mode + config ------------------------------------------------
;; A physarum theme may carry :p-defaults — parameter overrides applied to the
;; live params when you switch to it, giving each its own character. The agents
;; deposit COLOUR into the smoke in :smoke / :haze modes, and a white trail in
;; :trail mode. Modes that drive smoke from physarum agents:
;;   :smoke — agents steer toward their own colour => coloured NETWORKS (slime)
;;   :haze  — agents don't steer, they wander => diffuse coloured SMOKE
;;   :trail — classic white transport network on a separate trail map
(def summer [[1.0 0.05 0.70] [1.0 0.55 0.0] [0.20 1.0 0.25]])  ; the summerfest hues

(def themes
  {:jets    {:mode :sources
             :sources [{:color [1.0 0.15 0.10] :emit 1.0 :r 3 :motion {:type :brownian :base [0.35 0.55] :amp 0.0035}}
                       {:color [0.10 0.45 1.0] :emit 1.0 :r 3 :motion {:type :brownian :base [0.65 0.55] :amp 0.0035}}
                       {:color [0.20 1.0 0.30] :emit 0.8 :r 2 :motion {:type :brownian :base [0.50 0.50] :amp 0.0040}}]}
   :jet1    {:mode :sources   ; a SINGLE moving source; colour taken live from (:jet-color p)
             :sources [{:color [1.0 0.30 0.08] :emit 1.0 :r 3
                        :motion {:type :brownian :base [0.5 0.55] :amp 0.0035}}]}
   ;; --- physarum-driven smoke, variations ---------------------------------
   :slime   {:mode :smoke    ; agents steer toward own colour => coloured networks
             :palette summer
             :p-defaults {:p-count 3000 :p-sensor 9.0 :p-sense-angle 0.5 :p-turn 0.45
                          :p-speed 1.2 :p-deposit 0.2 :p-wind 0.2 :p-wander 0.0
                          :buoy 0.4 :keep 0.99 :expos 1.4}}
   :haze    {:mode :haze     ; the smoke ITSELF is wandering agents => diffuse smoke, no networks
             :palette summer
             :p-defaults {:p-count 6000 :p-speed 1.0 :p-deposit 0.10 :p-wind 0.8 :p-wander 0.7
                          :buoy 0.5 :keep 0.985 :expos 1.8}}
   :swarm   {:mode :haze     ; smoke SOURCES that are agents: few, bright, wandering emitters
             :palette summer
             :p-defaults {:p-count 600 :p-speed 1.8 :p-deposit 0.6 :p-wind 1.2 :p-wander 0.4
                          :buoy 0.7 :keep 0.985 :expos 1.5}}
   :rivers  {:mode :smoke    ; faint steering + wander => flowing strands between network and smoke
             :palette summer
             :p-defaults {:p-count 4000 :p-sensor 16.0 :p-sense-angle 0.6 :p-turn 0.12
                          :p-speed 1.3 :p-deposit 0.16 :p-wind 0.9 :p-wander 0.25
                          :buoy 0.5 :keep 0.99 :expos 1.5}}
   :network {:mode :trail
             :palette [[1.0 1.0 1.0]]
             :p-defaults {:p-count 3000 :p-sensor 9.0 :p-sense-angle 0.5 :p-turn 0.45
                          :p-speed 1.2 :p-deposit 0.2 :p-decay 0.90 :p-bright 0.6 :p-wander 0.0}}})

(defn theme-defaults
  "Per-theme parameter overrides (merged into params on theme switch); nil if none."
  [theme-kw]
  (:p-defaults (get themes theme-kw)))

;; the "summerfest" palette — the saturated hues used by the :slime theme
(def summerfest
  [[:pink   [1.0  0.05 0.70]]
   [:orange [1.0  0.55 0.0]]
   [:green  [0.20 1.0  0.25]]])

;; colour presets shown in the controls picker (for the single :jet1 source)
(def jet-palettes summerfest)

(def default-params
  {:dt          0.04     ; sim speed — lower = slower smoke
   :visc        0.0001   ; spectral viscosity (exp(-|k|^2 dt visc)); higher = smoother
   :buoy        0.4      ; buoyancy (rise speed), force per unit total density
   :keep        0.99     ; density kept per frame (<1 => soft fade)
   :edge-margin 1        ; sponge-border width (cells); fades flow at edges (walls)
   :blur-passes 0        ; render-only density blur (0 = crisp)
   :expos       1.4      ; tonemap exposure per colour channel (lower = keeps colour, less white-out)
   :wind        4.0      ; wind strength (noise flow-field force on the smoke)
   :noise-scale 2.0      ; wind spatial frequency
   :noise-speed 0.012    ; how fast the wind field evolves
   :theme       :slime   ; one of (keys themes)
   :jet-color   [1.0 0.30 0.08]  ; live colour of the single source in the :jet1 theme
   ;; --- "stars": bright colour dots flashing white at high-density peaks ---
   :stars       false
   :star-thresh 2.5      ; density (sum of channels) above which a peak sparks (higher = rarer/persistent)
   :star-radius 3        ; star radius in window pixels
   :star-speed  0.25     ; twinkle speed (white-flash lerp over time)
   ;; --- Physarum (used by :slime / :network themes) ---
   :p-count       3000   ; number of agents (more = finer, denser networks)
   :p-sensor      9.0    ; sensor offset distance (cells)
   :p-sense-angle 0.5    ; angle between the 3 sensors (rad)
   :p-turn        0.45   ; how hard agents steer (rad)
   :p-speed       1.2    ; agent step size (cells/tick)
   :p-deposit     0.2   ; deposit per agent (colour in :slime, white trail in :network)
   :p-wind        0.2    ; how much the fluid velocity drags the agents
   :p-wander      0.0    ; random heading jitter per tick (rad); high => smoke not networks (:haze)
   :p-decay       0.90   ; trail kept per tick (:network mode)
   :p-bright      0.6    ; white-network brightness (:network mode)
   :boids         nil})  ;; boids config (:sources mode); nil => boids/default-boid

(defn theme [p] (get themes (:theme p) (:jets themes)))
(defn mode  [p] (:mode (theme p)))

(defonce frame   (atom 0))    ; frame counter, drives motion + wind time
(defonce src-pos (atom nil))  ; {:theme kw :pos [...] :vel [...]} — live source state
(defonce stars   (atom []))   ; persistent star particles {:x :y :vx :vy :ax :ay :r :g :b}
(def ^:const STAR-MAX 700)
(def ^:const STAR-MINDIST2 100.0)  ; (10 px)^2 minimum spacing between stars

;; ---- moving coloured sources (:jets mode) ----------------------------------
(defn- bounce [^double x ^double v]
  (cond (< x 0.1) [0.1 (- v)] (> x 0.9) [0.9 (- v)] :else [x v]))

(defn- step-source
  "Advance one source -> [[x y] [vx vy]]. Brownian integrates a random + boid
   acceleration into velocity (damped) into position; scripted motions ignore it."
  [src [px py] [vx vy] [bax bay] t]
  (let [t (double t) m (:motion src)]
    (if (= (:type m) :brownian)
      (let [acc  (double (:amp m 0.0025)) damp (double (:damp m 0.92))
            vx (* damp (+ (double vx) (* acc 2.0 (- (double (rand)) 0.5)) (double bax)))
            vy (* damp (+ (double vy) (* acc 2.0 (- (double (rand)) 0.5)) (double bay)))
            [px vx] (bounce (+ (double px) vx) vx)
            [py vy] (bounce (+ (double py) vy) vy)]
        [[px py] [vx vy]])
      (let [base (:base m) bx (double (first base)) by (double (second base))
            amp (double (:amp m 0.0)) speed (double (:speed m 0.0)) phase (double (:phase m 0.0))
            ph (+ (* speed t) phase)]
        [(case (:type m)
           :osc-x  [(+ bx (* amp (Math/sin ph))) by]
           :osc-y  [bx (+ by (* amp (Math/sin ph)))]
           :circle [(+ bx (* amp (Math/cos ph))) (+ by (* amp (Math/sin ph)))]
           [bx by])
         [0.0 0.0]]))))

(defn- emit-sources! [fl srcs positions]
  (let [n (long N)
        ^floats dr (:dr fl) ^floats dg (:dg fl) ^floats db (:db fl)]
    (doseq [[src pos] (map vector srcs positions)]
      (let [color (:color src) e (double (:emit src))
            er (float (* e (double (nth color 0)))) eg (float (* e (double (nth color 1))))
            eb (float (* e (double (nth color 2))))
            cx (long (* (double (first pos)) n)) cy (long (* (double (second pos)) n))
            r (long (:r src))]
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

(defn- emit-jets! [fl p t]
  (let [srcs (:sources (theme p))]
    (when (or (nil? (:vel @src-pos))
              (not= (count (:pos @src-pos)) (count srcs))
              (not= (:theme p) (:theme @src-pos)))
      (reset! src-pos {:theme (:theme p)
                       :pos (mapv (comp :base :motion) srcs)
                       :vel (mapv (constantly [0.0 0.0]) srcs)}))
    (let [accels    (boids/accelerations (:pos @src-pos) (:vel @src-pos) (:boids p))
          steps     (mapv #(step-source %1 %2 %3 %4 t) srcs (:pos @src-pos) (:vel @src-pos) accels)
          positions (mapv first steps)]
      (reset! src-pos {:theme (:theme p) :pos positions :vel (mapv second steps)})
      ;; :jet1 = single source whose colour is steered live via (:jet-color p)
      (let [srcs (if (and (= (:theme p) :jet1) (:jet-color p))
                   (mapv #(assoc % :color (:jet-color p)) srcs)
                   srcs)]
        (emit-sources! fl srcs positions)))))

;; ---- noise flow-field wind -------------------------------------------------
(defn- apply-wind! [fl p ^double t]
  (let [n (long N)
        ^floats fx (:fx fl) ^floats fy (:fy fl) ^floats d (:dens fl)
        w (double (:wind p)) sc (* TAU (double (:noise-scale p)))
        sp (* (double (:noise-speed p)) t) inv (/ 1.0 (double n))]
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
  "Per-frame pre-step: reset forces, apply wind, and (in :jets mode) emit the
   moving coloured sources. Physarum modes emit inside `advance`."
  [fl p]
  (f/clear-forces! fl)
  (let [t (double (swap! frame inc))]
    (when (= (mode p) :sources) (emit-jets! fl p t))
    (apply-wind! fl p t))
  fl)

(declare update-stars!)

(defn new-fluid
  "Fresh state: fluid grid + Physarum agents coloured from the theme palette."
  [p]
  (reset! stars [])
  (assoc (f/make-fluid N)
         :phys (phys/make N (:p-count p) (or (:palette (theme p)) [[1.0 1.0 1.0]]))))

(defn advance
  "One tick: velocity → (Physarum emission for :slime/:network) → advect/dissipate
   → spawn/drift persistent stars."
  [fl p]
  (let [fl (f/vel-step fl (:visc p) (:dt p) (:buoy p))
        m  (mode p)]
    (when (#{:smoke :trail :haze} m) (phys/step! (:phys fl) fl (assoc p :p-mode m)))
    (let [fl (-> fl
                 (f/advect-colors! (:dt p))
                 (f/dissipate-colors! (:keep p))
                 (f/edge-fade! (:edge-margin p))
                 (f/compute-total!))]
      (when (:stars p) (update-stars! fl p))
      fl)))

;; ---- render ----------------------------------------------------------------
(def ^:const TLUTN 512)
(def ^:const CHMAX 3.0)

(defn- tone-lut ^floats [expos]
  (let [lut (float-array TLUTN) e (double expos)]
    (dotimes [i TLUTN]
      (let [ch (* (/ (double i) (dec TLUTN)) CHMAX)]
        (aset lut i (float (* 255.0 (- 1.0 (Math/exp (- (* e ch)))))))))
    lut))

(defn- update-stars!
  "Drift existing stars by their small constant acceleration; a star dies once it
   drifts to the window edge. Then spawn a new persistent star at each strong
   density peak that has no star nearby yet. Runs in `advance` (works headless)."
  [fl p]
  (let [n (long N) wmax (double (dec W))
        ^floats dr (:dr fl) ^floats dg (:dg fl) ^floats db (:db fl)
        thresh (double (:star-thresh p))
        g->px (/ (double (dec W)) (double (dec n)))
        moved (into [] (keep (fn [s]
                               (let [vx (+ (double (:vx s)) (double (:ax s)))
                                     vy (+ (double (:vy s)) (double (:ay s)))
                                     nx (+ (double (:x s)) vx)
                                     ny (+ (double (:y s)) vy)]
                                 ;; die at the edge instead of clamping
                                 (when (and (> nx 0.0) (< nx wmax) (> ny 0.0) (< ny wmax))
                                   (assoc s :vx vx :vy vy :x nx :y ny)))))
                    @stars)
        acc (volatile! moved)]
    (dotimes [j n]
      (dotimes [i n]
        (when (and (pos? i) (< i (dec n)) (pos? j) (< j (dec n)) (< (count @acc) STAR-MAX))
          (let [k (f/idx n i j)
                d (+ (aget dr k) (aget dg k) (aget db k))]
            (when (and (> d thresh)
                       (>= d (+ (aget dr (f/idx n (dec i) j)) (aget dg (f/idx n (dec i) j)) (aget db (f/idx n (dec i) j))))
                       (>= d (+ (aget dr (f/idx n (inc i) j)) (aget dg (f/idx n (inc i) j)) (aget db (f/idx n (inc i) j))))
                       (>= d (+ (aget dr (f/idx n i (dec j))) (aget dg (f/idx n i (dec j))) (aget db (f/idx n i (dec j)))))
                       (>= d (+ (aget dr (f/idx n i (inc j))) (aget dg (f/idx n i (inc j))) (aget db (f/idx n i (inc j))))))
              (let [px (* i g->px) py (* j g->px)]
                (when (not-any? (fn [s]
                                  (let [ex (- px (double (:x s))) ey (- py (double (:y s)))]
                                    (< (+ (* ex ex) (* ey ey)) STAR-MINDIST2)))
                                @acc)
                  (let [inv (/ 1.0 d)]
                    (vswap! acc conj
                            {:x px :y py
                             :vx (* 0.5 (- (double (rand)) 0.5)) :vy (* 0.5 (- (double (rand)) 0.5))
                             :ax (* 0.03 (- (double (rand)) 0.5)) :ay (* 0.03 (- (double (rand)) 0.5))
                             :r (* (aget dr k) inv) :g (* (aget dg k) inv) :b (* (aget db k) inv)})))))))))
    (reset! stars @acc)))

(defn- draw-stars!
  "Draw each persistent star as a bright colour disc, lerped toward white by a
   per-star time twinkle."
  [^ints px p ^double t]
  (let [w (long W) R (long (:star-radius p)) speed (double (:star-speed p))]
    (doseq [s @stars]
      (let [cx (long (:x s)) cy (long (:y s))
            cr0 (double (:r s)) cg0 (double (:g s)) cb0 (double (:b s))
            flash (+ 0.5 (* 0.5 (Math/sin (+ (* t speed) (* (double (:x s)) 0.05)))))
            rr (+ cr0 (* (- 1.0 cr0) flash))
            gg (+ cg0 (* (- 1.0 cg0) flash))
            bbq (+ cb0 (* (- 1.0 cb0) flash))]
        (dotimes [dy (inc (* 2 R))]
          (let [oy (- dy R) py (+ cy oy)]
            (when (and (>= py 0) (< py w))
              (dotimes [dx (inc (* 2 R))]
                (let [ox (- dx R) pxx (+ cx ox) rr2 (+ (* ox ox) (* oy oy))]
                  (when (and (<= rr2 (* R R)) (>= pxx 0) (< pxx w))
                    (let [fall (- 1.0 (/ (Math/sqrt (double rr2)) (inc R)))
                          ri (long (* 255.0 (min 1.0 (* rr fall))))
                          gi (long (* 255.0 (min 1.0 (* gg fall))))
                          bi (long (* 255.0 (min 1.0 (* bbq fall))))
                          ix (+ pxx (* py w)) old (aget px ix)
                          nr (min 255 (+ ri (bit-and (bit-shift-right old 16) 0xFF)))
                          ng (min 255 (+ gi (bit-and (bit-shift-right old 8) 0xFF)))
                          nb (min 255 (+ bi (bit-and old 0xFF)))]
                      (aset px ix (unchecked-int (bit-or (unchecked-int 0xFF000000)
                                                         (bit-shift-left nr 16) (bit-shift-left ng 8) nb))))))))))))))

(defn render-pixels!
  "Composite the three tonemapped colour channels; in :network mode also add the
   white trail; optionally stamp twinkling stars at high-density peaks."
  [fl p ^ints px]
  (f/blur-colors! fl (:blur-passes p))
  (let [n      (long N)
        w      (long W)
        nm1    (dec n)
        ^floats br (:br fl) ^floats bg (:bg fl) ^floats bb (:bb fl)
        ^floats trail (:trail (:phys fl))
        netw   (double (if (= (mode p) :trail) (:p-bright p) 0.0))
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
                tr (* netw (+ (* sy0 (+ (* sx0 (aget trail k00)) (* fx (aget trail k10))))
                              (* fy  (+ (* sx0 (aget trail k01)) (* fx (aget trail k11))))))
                cr (+ (* sy0 (+ (* sx0 (aget br k00)) (* fx (aget br k10))))
                      (* fy  (+ (* sx0 (aget br k01)) (* fx (aget br k11)))) tr)
                cg (+ (* sy0 (+ (* sx0 (aget bg k00)) (* fx (aget bg k10))))
                      (* fy  (+ (* sx0 (aget bg k01)) (* fx (aget bg k11)))) tr)
                cb (+ (* sy0 (+ (* sx0 (aget bb k00)) (* fx (aget bb k10))))
                      (* fy  (+ (* sx0 (aget bb k01)) (* fx (aget bb k11)))) tr)
                ri (long (aget tl (min (dec TLUTN) (max 0 (long (* cr tscale))))))
                gi (long (aget tl (min (dec TLUTN) (max 0 (long (* cg tscale))))))
                bi (long (aget tl (min (dec TLUTN) (max 0 (long (* cb tscale))))))]
            (aset px (+ ox row)
                  (unchecked-int (bit-or (unchecked-int 0xFF000000)
                                         (bit-shift-left ri 16) (bit-shift-left gi 8) bi)))))))
    (when (:stars p) (draw-stars! px p (double @frame)))
    px))
