(ns smoke.boids
  "A thin flocking layer over the moving sources. Each source is treated as a
   boid that feels the three classic Reynolds steering rules from its neighbours
   (the *other* sources within a perception radius):

     separation — steer away from neighbours that crowd it
     alignment  — steer toward the neighbours' average heading (velocity)
     cohesion   — steer toward the neighbours' centre of mass

   The combined steering is returned as a small ACCELERATION (per source), which
   `smoke.scene` folds subtly into each source's motion. So the emitting jets
   drift in loose coordination — easing apart, falling into step, regrouping —
   instead of wandering wholly independently.

   Everything here is in the same normalized [0,1] source-position space used by
   the scene; there are only a handful of sources, so this stays plain Clojure
   (no primitive arrays).")

(def default-boid
  "Subtle by design: `strength` scales the whole steering vector and `max-accel`
   HARD-CAPS the per-frame result. The scene integrates this accel into velocity
   with damping (~0.92), so terminal velocity ≈ accel/0.08 — without the cap a
   large cohesion pull (which grows with distance) builds up runaway speed. The
   brownian random accel is ~0.0035/frame, so a cap near that keeps boids from
   dominating the motion."
  {:radius    0.5      ; perception radius (normalized units); ignore farther sources
   :sep-dist  0.16     ; closer than this => separation pushes apart (1/d falloff)
   :sep       0.5      ; separation weight
   :align     0.5      ; alignment weight (match neighbours' velocity)
   :cohere    0.8      ; cohesion weight (drift toward neighbours' centre)
   :strength  0.050    ; overall scale on the steering vector
   :max-accel 0.002})  ; hard cap on |accel| per frame (below brownian's 0.0035)

(defn- v+ [[ax ay] [bx by]] [(+ (double ax) (double bx)) (+ (double ay) (double by))])
(defn- v- [[ax ay] [bx by]] [(- (double ax) (double bx)) (- (double ay) (double by))])
(defn- v* [[ax ay] s]       [(* (double ax) (double s)) (* (double ay) (double s))])
(defn- mag ^double [[ax ay]] (Math/sqrt (+ (* (double ax) (double ax)) (* (double ay) (double ay)))))

(defn accelerations
  "Given source positions `[[x y] ...]` and matching velocities `[[vx vy] ...]`
   (normalized units) plus a config `cfg` (see `default-boid`), return a vector
   of `[ax ay]` steering accelerations, one per source. Sources with no
   neighbour inside the perception radius get `[0.0 0.0]`."
  [positions velocities cfg]
  (let [cfg      (merge default-boid cfg)
        radius   (double (:radius cfg))
        sep-dist (double (:sep-dist cfg))
        sepW     (double (:sep cfg))
        alignW   (double (:align cfg))
        cohW     (double (:cohere cfg))
        strength (double (:strength cfg))
        maxa     (double (:max-accel cfg))
        n        (count positions)]
    (vec
     (for [i (range n)]
       (let [pi    (nth positions i)
             nbrs  (for [j (range n)
                         :when (not= i j)
                         :let  [d (mag (v- pi (nth positions j)))]
                         :when (< d radius)]
                     [j d])]
         (if (empty? nbrs)
           [0.0 0.0]
           (let [k    (double (count nbrs))
                 ;; separation: away from crowding neighbours, with 1/d falloff
                 sep  (reduce (fn [acc [j d]]
                                (if (< (double d) sep-dist)
                                  (v+ acc (v* (v- pi (nth positions j))
                                              (/ 1.0 (max (double d) 1.0e-4))))
                                  acc))
                              [0.0 0.0] nbrs)
                 ;; cohesion: toward the neighbours' centre of mass
                 ctr  (v* (reduce (fn [acc [j _]] (v+ acc (nth positions j))) [0.0 0.0] nbrs)
                          (/ 1.0 k))
                 coh  (v- ctr pi)
                 ;; alignment: toward the neighbours' average velocity
                 avgv (v* (reduce (fn [acc [j _]] (v+ acc (nth velocities j))) [0.0 0.0] nbrs)
                          (/ 1.0 k))
                 steer (-> (v* sep  sepW)
                           (v+ (v* coh  cohW))
                           (v+ (v* avgv alignW))
                           (v* strength))
                 ;; hard-cap |accel| so a big cohesion pull can't build runaway speed
                 m     (mag steer)]
             (if (> m maxa) (v* steer (/ maxa m)) steer))))))))
