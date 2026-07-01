(ns smoke.flock
  "A flock of small balls (boids) that ARE the smoke source.

   Each ball flocks by the three classic Reynolds rules among its neighbours —

     separation — steer away from balls that crowd it
     alignment  — steer toward the neighbours' average velocity
     cohesion   — steer toward the neighbours' centre of mass

   — is dragged by the fluid wind (`:flock-wind`), and DEPOSITS ITS COLOUR into
   the smoke as it moves. The fluid then advects + dissipates that colour, so the
   moving flock leaves smoke behind it. Tuning the deposit footprint and how hard
   the balls also stir the fluid (`:flock-flow`) sweeps the look from diffuse
   HAZE (big soft footprint, no flow) through flowing RIVERS (tiny footprint,
   strong flow) to SLIME-like strands in between.

   Unlike `smoke.boids` — a subtle steering layer over a few sources in [0,1]
   space — this is a full flock of hundreds of agents living in grid-cell space
   on float arrays, with a spatial-hash grid so neighbour queries stay ~O(n·k)
   instead of O(n^2). The domain is periodic (toroidal), matching the solver."
  (:require [smoke.fluid :as f]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(def ^:const PI2 6.283185307179586)

(defn- hsv->rgb [h s v]
  (let [rgb (java.awt.Color/HSBtoRGB (float h) (float s) (float v))]
    [(/ (bit-and (bit-shift-right rgb 16) 0xff) 255.0)
     (/ (bit-and (bit-shift-right rgb 8) 0xff) 255.0)
     (/ (bit-and rgb 0xff) 255.0)]))

(defn make
  "Allocate `cnt` balls (random pos + random initial velocity), coloured from
   `palette` (round-robin by band index). `tvx`/`tvy` are scratch arrays for the
   read-old/write-new velocity update so flocking is order-independent."
  [^long n ^long cnt palette]
  (let [xs (float-array cnt) ys (float-array cnt)
        vx (float-array cnt) vy (float-array cnt)
        tvx (float-array cnt) tvy (float-array cnt)
        ar (float-array cnt) ag (float-array cnt) ab (float-array cnt)
        band (int-array cnt)
        pal (vec palette) pc (max 1 (count pal))]
    (dotimes [i cnt]
      (aset xs i (float (* (double (rand)) n)))
      (aset ys i (float (* (double (rand)) n)))
      (let [a (* PI2 (double (rand))) s (+ 0.4 (double (rand)))]
        (aset vx i (float (* s (Math/cos a))))
        (aset vy i (float (* s (Math/sin a)))))
      (aset band i (int (mod i pc)))
      (let [c (nth pal (mod i pc))]
        (aset ar i (float (nth c 0)))
        (aset ag i (float (nth c 1)))
        (aset ab i (float (nth c 2)))))
    {:n n :count cnt :xs xs :ys ys :vx vx :vy vy :tvx tvx :tvy tvy
     :ar ar :ag ag :ab ab :band band}))

(defn recolor!
  "Re-paint every ball from `palette` by its band index (mutates in place)."
  [flock palette]
  (let [cnt (long (:count flock))
        ^floats ar (:ar flock) ^floats ag (:ag flock) ^floats ab (:ab flock)
        ^ints band (:band flock)
        pal (vec palette) pc (count pal)]
    (when (pos? pc)
      (dotimes [i cnt]
        (let [c (nth pal (mod (aget band i) pc))]
          (aset ar i (float (nth c 0)))
          (aset ag i (float (nth c 1)))
          (aset ab i (float (nth c 2))))))
    flock))

(defn- wrap ^double [^double x ^long n]
  (let [m (rem x n)] (if (neg? m) (+ m n) m)))

;; shortest toroidal delta from a to b on a length-n ring, in [-n/2, n/2)
(defn- tdelta ^double [^double a ^double b ^long n]
  (let [d (- b a) h (* 0.5 n)]
    (cond (> d h) (- d n) (< d (- h)) (+ d n) :else d)))

(defn step!
  "Advance the flock one tick: build a spatial grid, compute each ball's Reynolds
   steering from its neighbours, fold in the fluid wind, integrate (speed-clamped)
   velocity + position, then deposit colour (and optionally stir the fluid)."
  [flock fl p]
  (let [n (long (:n flock)) cnt (long (:count flock))
        ^floats xs (:xs flock) ^floats ys (:ys flock)
        ^floats vx (:vx flock) ^floats vy (:vy flock)
        ^floats tvx (:tvx flock) ^floats tvy (:tvy flock)
        ^floats ar (:ar flock) ^floats ag (:ag flock) ^floats ab (:ab flock)
        ^floats u (:u fl) ^floats v (:v fl)
        ^floats dr (:dr fl) ^floats dg (:dg fl) ^floats db (:db fl)
        radius  (double (:flock-radius p 14.0))
        rad2    (* radius radius)
        sepd    (double (:flock-sep-dist p 6.0))
        sepd2   (* sepd sepd)
        sepW    (double (:flock-sep p 1.4))
        alignW  (double (:flock-align p 0.7))
        cohW    (double (:flock-cohere p 0.55))
        maxa    (double (:flock-accel p 0.5))
        maxs    (double (:flock-max-speed p 2.2))
        mins    (double (:flock-min-speed p 0.6))
        damp    (double (:flock-damp p 0.96))
        windk   (double (:flock-wind p 0.6))
        dep     (double (:flock-deposit p 0.5))
        rdep    (double (:flock-dep-radius p 1.6))
        flowk   (double (:flock-flow p 0.0))         ; >0 => stir fluid toward velocity (rivers)
        flowa   (double (:flock-flow-blend p 0.35))
        flow?   (pos? flowk)
        ;; FREQ-REACT: split the flock into one BUCKET per frequency band; each band's
        ;; gain boosts that bucket's max speed (:flock-freq-speed) + deposit (:flock-freq-deposit)
        ^doubles gains (:audio-gains p)
        freq?   (boolean (and gains (:flock-freq-react? p)))
        nb      (long (if gains (alength gains) 1))
        fsa     (double (:flock-freq-speed p 0.0))
        fda     (double (:flock-freq-deposit p 0.0))
        ;; spatial-hash grid: one bucket per `radius`-sized square (toroidal)
        bs      (long (max 1 (long radius)))
        gn      (long (max 1 (quot n bs)))
        gn2     (* gn gn)
        head    (int-array gn2 -1)
        nxt     (int-array cnt -1)]
    (dotimes [i cnt]
      (let [bx (long (mod (quot (long (aget xs i)) bs) gn))
            by (long (mod (quot (long (aget ys i)) bs) gn))
            b  (+ bx (* by gn))]
        (aset nxt i (aget head b))
        (aset head b i)))
    ;; pass 1: read OLD state, compute each ball's new velocity into tvx/tvy
    (dotimes [i cnt]
      (let [xi (double (aget xs i)) yi (double (aget ys i))
            vxi (double (aget vx i)) vyi (double (aget vy i))
            bx (long (mod (quot (long xi) bs) gn))
            by (long (mod (quot (long yi) bs) gn))]
        (let [acc (double-array 7)]   ; [sx sy ax ay cx cy k]
          (loop [gj -1]
            (when (<= gj 1)
              (loop [gi -1]
                (when (<= gi 1)
                  (let [bbx (long (mod (+ bx gi) gn)) bby (long (mod (+ by gj) gn))
                        b   (+ bbx (* bby gn))]
                    (loop [j (aget head b)]
                      (when (>= j 0)
                        (when (not= j i)
                          (let [dx (tdelta xi (double (aget xs j)) n)
                                dy (tdelta yi (double (aget ys j)) n)
                                d2 (+ (* dx dx) (* dy dy))]
                            (when (< d2 rad2)
                              (when (and (< d2 sepd2) (> d2 1.0e-6))
                                ;; separation: away from the neighbour, 1/d falloff
                                (let [inv (/ 1.0 (Math/sqrt d2))]
                                  (aset acc 0 (- (aget acc 0) (* dx inv)))
                                  (aset acc 1 (- (aget acc 1) (* dy inv)))))
                              (aset acc 2 (+ (aget acc 2) (double (aget vx j))))
                              (aset acc 3 (+ (aget acc 3) (double (aget vy j))))
                              (aset acc 4 (+ (aget acc 4) dx))   ; toroidal offset to neighbour
                              (aset acc 5 (+ (aget acc 5) dy))
                              (aset acc 6 (+ (aget acc 6) 1.0)))))
                        (recur (aget nxt j)))))
                  (recur (inc gi))))
              (recur (inc gj))))
          (let [k (aget acc 6)
                stx (if (pos? k)
                      (+ (* (aget acc 0) sepW)
                         (* (- (/ (aget acc 2) k) vxi) alignW)   ; alignment: match avg velocity
                         (* (/ (aget acc 4) k) cohW))            ; cohesion: toward centre offset
                      0.0)
                sty (if (pos? k)
                      (+ (* (aget acc 1) sepW)
                         (* (- (/ (aget acc 3) k) vyi) alignW)
                         (* (/ (aget acc 5) k) cohW))
                      0.0)
                ;; cap |steer| to maxa so a strong cohesion pull can't run away
                sm (Math/sqrt (+ (* stx stx) (* sty sty)))
                [stx sty] (if (> sm maxa) [(* stx (/ maxa sm)) (* sty (/ maxa sm))] [stx sty])
                ;; wind drag toward the local fluid velocity
                wk (f/idx n (long (wrap xi n)) (long (wrap yi n)))
                nvx (* damp (+ vxi (double stx) (* windk (double (aget u wk)))))
                nvy (* damp (+ vyi (double sty) (* windk (double (aget v wk)))))
                spd (Math/sqrt (+ (* nvx nvx) (* nvy nvy)))
                ;; this ball's band gain lifts its speed ceiling => its band surges on the beat
                gband (if freq? (aget gains (min (dec nb) (quot (* i nb) cnt))) 0.0)
                maxi (* maxs (+ 1.0 (* fsa gband)))
                ;; keep the flock alive (>= mins) but tame (<= maxi)
                sc  (double (cond (> spd maxi) (/ maxi spd)
                                  (and (> spd 1.0e-4) (< spd mins)) (/ mins spd)
                                  :else 1.0))]
            (aset tvx i (float (* nvx sc)))
            (aset tvy i (float (* nvy sc)))))))
    ;; pass 2: commit velocity, move, deposit colour, optionally stir the fluid
    (let [ri (long (Math/ceil (max 0.0 rdep)))]
      (dotimes [i cnt]
        (let [nvx (double (aget tvx i)) nvy (double (aget tvy i))
              nx (wrap (+ (double (aget xs i)) nvx) n)
              ny (wrap (+ (double (aget ys i)) nvy) n)
              cr (double (aget ar i)) cg (double (aget ag i)) cb (double (aget ab i))
              ;; band gain lifts this ball's deposit => its colour blooms on the beat
              gband (if freq? (aget gains (min (dec nb) (quot (* i nb) cnt))) 0.0)
              dep (* dep (+ 1.0 (* fda gband)))
              ci (long nx) cj (long ny)]
          (aset vx i (float nvx)) (aset vy i (float nvy))
          (aset xs i (float nx))  (aset ys i (float ny))
          (if (<= rdep 1.0)
            ;; tiny footprint => near-point source the fluid smears into filaments
            (let [k (f/idx n ci cj)]
              (aset dr k (+ (aget dr k) (float (* dep cr))))
              (aset dg k (+ (aget dg k) (float (* dep cg))))
              (aset db k (+ (aget db k) (float (* dep cb)))))
            ;; soft disc with linear falloff => diffuse smoke / haze
            (let [inv (/ 1.0 (double ri))]
              (loop [oj (- ri)]
                (when (<= oj ri)
                  (loop [oi (- ri)]
                    (when (<= oi ri)
                      (let [d (Math/sqrt (double (+ (* oi oi) (* oj oj))))]
                        (when (<= d rdep)
                          (let [g (* dep (- 1.0 (* d inv)))
                                k (f/idx n (long (mod (+ ci oi) n)) (long (mod (+ cj oj) n)))]
                            (aset dr k (+ (aget dr k) (float (* g cr))))
                            (aset dg k (+ (aget dg k) (float (* g cg))))
                            (aset db k (+ (aget db k) (float (* g cb)))))))
                      (recur (inc oi))))
                  (recur (inc oj))))))
          (when flow?
            ;; BLEND the fluid velocity toward a flowk-scaled unit of the ball's
            ;; heading (bounded => |u|≈flowk, no blow-up — same trick as the
            ;; physarum :flow network) so the smoke flows along the ball's path
            (let [spd (Math/sqrt (+ (* nvx nvx) (* nvy nvy)))
                  inv (/ 1.0 (max 1.0e-4 spd))
                  tx  (* flowk nvx inv) ty (* flowk nvy inv)
                  k   (f/idx n ci cj)]
              (aset u k (float (+ (aget u k) (* flowa (- tx (aget u k))))))
              (aset v k (float (+ (aget v k) (* flowa (- ty (aget v k))))))))))))
  flock)

(comment
  ;; ---- the :boids flock, live ------------------------------------------------
  ;; The flock IS the smoke source: small balls flock (Reynolds) + ride the wind +
  ;; deposit their colour. Two knobs sweep the look:
  ;;   :flock-dep-radius  <=1 => point/filament ribbons ;  big => diffuse HAZE
  ;;   :flock-flow        >0  => balls stir the fluid toward their heading => RIVERS
  ;; Presets in smoke.scene: :boids :boid-rivers :boid-haze :boid-reactive

  ;; --- drive the LIVE Quil window (after a clj -M:nrepl) ---
  (require '[smoke.core :as core] '[smoke.audio :as audio])
  (swap! core/params merge (smoke.scene/preset-params :boid-reactive))
  (do
    (core/start! :size [960 960])
    ;; (audio/start! "media/music/brejcha.wav")
    ;; (audio/start! "media/music/dave-dinger.wav")
    ;; (audio/start! "/home/benj/repos/musicanalysis/alle-warten.wav")
    (audio/start! "/home/benj/repos/musicanalysis/d-neuland-vom-feisten-i-chaos.wav"))

;; flock reacts: per-band buckets surge/bloom on beats

  ;; tweak any knob live — applies next frame (no reload needed)
  (swap! core/params assoc :flock-ball-r 4 :flock-flow 6.0 :flock-count 400)
  (swap! core/params assoc :flock-dep-radius 4.0 :flock-deposit 0.12 :flock-flow 0.0) ; => haze
  (reset! core/reset? true) ; re-seed the field + flock with current params
  ;; GOTCHA: reloading scene/core under the live window can freeze the GL loop —
  ;; recover with (core/restart!) then re-apply params + (audio/start! ...).

  ;; --- headless stills (no window) ---
  (require '[smoke.headless :as h])
  (h/snap 220 "/tmp/boids.png" (smoke.scene/preset-params :boid-rivers))

  ;; --- offline audio-reactive MP4 (separate clj process!) ---
  (require '[smoke.video :as v])
  (v/render! "/tmp/boids.mp4" :audio "media/music/brejcha.wav"
             :preset :boid-reactive :render [1280 720])

  ;; --- REEL: a persistent THICK smoke CIRCLE in the middle, cyber-flock around it ---
  ;; The centre circle holds every frame (steps 1 + rest 0 => no grow/rest cycle) at a
  ;; fixed radius; fat :pulse-line-width + :pulse-amount => a thick continuous smoke ring.
  ;; The :cyber-flock preset supplies the neon slime+flock currents swirling around it.
  ;;   thicker ring  => raise :pulse-line-width / :pulse-amount
  ;;   bigger circle => raise :pulse-shape-size (radius, cells)
  ;;   flip colours  => :audio-color-cycle 8 (else the ring stays the fixed :pulse-color)
  (v/reel! "media/reels/ceciliaasoro-cyberflock-thickcircle.mp4"
           :audio "media/music/ceciliaasoro.wav"
           :seconds 13 :grid 512 :sharpen 0.5
           :preset :cyber-flock
           :params {:pulse-shape :circle
                    :pulse-shape-steps 1        ;; single fixed size (no growing)
                    :pulse-shape-rest-beats 0   ;; never rest => circle shows every frame
                    :pulse-shape-edge-beats 16  ;; beats before a fresh (same) circle
                    :pulse-shape-size 120       ;; circle radius (cells)
                    :pulse-line-width 18        ;; THICK ring
                    :pulse-amount 0.6           ;; continuous dense deposit
                    :pulse-color [0.6 0.95 1.0] ;; cyan ring
                    :audio-color-cycle 0        ;; stable colour (8 => flips)
                    :keep 0.92})

  ;; --- RING, blue/green ("die ist geil", Benni 2026-07-01) -------------------
  ;; Thin open circle whose colour GLIDES with the audio frequency (spectral
  ;; centroid, low-passed so it doesn't flicker), over a bright ocean-palette boid
  ;; network. The circle's own smoke deposit swells with the audio energy.
  ;;   :pulse-color-freq? + hue-base/span/smooth => freq-driven ring hue (green<->blue arc)
  ;;   :pulse-shape-audio-amp => ring smoke pumps harder on loud/beaty bits
  ;;   :audio-palette-set :ocean => blue/green network ; :p-bright 1.3 => bright net
  ;; Saved: media/reels/ceciliaasoro-cyberflock-ring-bluegreen.mp4
  (v/reel! "media/reels/ceciliaasoro-cyberflock-ring-bluegreen.mp4"
           :audio "media/music/ceciliaasoro.wav"
           :seconds 13 :grid 512 :sharpen 0.5
           :preset :cyber-flock
           :params {:pulse-shape :circle :pulse-shape-steps 1 :pulse-shape-rest-beats 0
                    :pulse-shape-edge-beats 16 :pulse-shape-size 70 :pulse-line-width 9
                    :pulse-amount 0.35 :pulse-shape-audio-amp 6.0   ;; ring smoke swells with energy
                    :pulse-color-freq? true :pulse-color-hue-base 0.33 :pulse-color-hue-span 0.33
                    :pulse-color-sat 1.0 :pulse-color-smooth 0.08   ;; freq hue, green<->blue, slow glide
                    :audio-palette-set :ocean :audio-color-cycle 0  ;; blue/green network
                    :flock-count 550 :flock-deposit 0.9 :p-bright 1.3 ;; more + brighter boid smoke
                    :wind 0.5 :keep 0.96})

  ;; --- RING + SWIRL — the KEEPER, POSTED ("swirl ist geil, das poste ich", Benni 2026-07-01)
  ;; A glowing cyan ring that INJECTS fluid velocity along its outline (see
  ;; stamp-shape-outline!): :pulse-shape-swirl spins the flock into a vortex around it
  ;; and :pulse-shape-push shoves the smoke outward, carving a clean void inside.
  ;;   negative :pulse-shape-push => pull smoke INWARD instead (vortex fills)
  ;;   :pulse-shape-on-onset? + :pulse-shape-cooldown-beats => ring blooms per vocal onset
  ;; DOUBLE length (26s, audio looped 2x via ceciliaasoro-x2.wav) so IG's 2nd loop has
  ;; real video (short 13s reels froze on IG's replay). Palette LOCKED to :cyan-purple
  ;; (cyan with a few purple) so the network matches the posted look, not random hues.
  ;; Posted: media/reels/ceciliaasoro-cyberflock-ring-swirl.mp4
  (v/reel! "media/reels/ceciliaasoro-cyberflock-ring-swirl.mp4"
           :audio "media/music/ceciliaasoro-x2.wav" ;; 2x loop of ceciliaasoro.wav
           :seconds 26 :grid 512 :sharpen 0.5
           :preset :cyber-flock
           :params {:pulse-shape :circle :pulse-shape-steps 1 :pulse-shape-rest-beats 0
                    :pulse-shape-edge-beats 16 :pulse-shape-size 80 :pulse-line-width 10
                    :pulse-amount 0.6 :pulse-color [0.6 0.95 1.0]
                    :pulse-shape-swirl 2.5 :pulse-shape-push 0.5 ;; ring stirs the flock into a vortex
                    :audio-palette-set :cyan-purple :audio-color-cycle 0 :keep 0.92}))
