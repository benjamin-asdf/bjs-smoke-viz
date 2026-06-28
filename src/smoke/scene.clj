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

(def ^:const N     256)        ; DEFAULT n x n periodic grid; override per-run via (:grid-n p).
                               ; The solver/scene read the live grid from the fluid (:n fl), so a
                               ; bigger grid = finer sim detail (cost ~ n^2 log n, now parallelised).
(def ^:const SCALE 3)          ; default render px per cell (=> 768x768 square) when no :render-w set.
(def ^:const W     (* N SCALE))
(def ^:const TAU   6.283185307179586)

(defn grid-n
  "Sim grid size for this run: (:grid-n p) or the default N."
  ^long [p] (long (:grid-n p N)))

(defn render-w
  "Output render width in px: (:render-w p) or a square default (grid-n * SCALE)."
  ^long [p] (long (:render-w p (* (grid-n p) SCALE))))

(defn render-h
  "Output render height in px: (:render-h p) or square (= render-w)."
  ^long [p] (long (:render-h p (render-w p))))

(defn win->cell
  "Map a window pixel (mx,my) in a (ww x wh) window to a grid cell [i j],
   inverting the render's scale-to-window + cover-crop. Returns clamped longs."
  [p ww wh mx my]
  (let [ww (long ww) wh (long wh) mx (long mx) my (long my)
        n (grid-n p) rw (render-w p) rh (render-h p) bigm (max rw rh)
        rx (* (double mx) (/ (double rw) (double (max 1 ww))))   ; window px -> render px
        ry (* (double my) (/ (double rh) (double (max 1 wh))))
        gscale (/ (double (dec n)) (double (dec bigm)))
        offx (* 0.5 (- (double bigm) (double rw)))
        offy (* 0.5 (- (double bigm) (double rh)))
        i (long (* (+ rx offx) gscale))
        j (long (* (+ ry offy) gscale))]
    [(min (dec n) (max 0 i)) (min (dec n) (max 0 j))]))

;; ---- themes = mode + config ------------------------------------------------
;; A physarum theme may carry :p-defaults — parameter overrides applied to the
;; live params when you switch to it, giving each its own character. The agents
;; deposit COLOUR into the smoke in :smoke / :haze modes, and a white trail in
;; :trail mode. Modes that drive smoke from physarum agents:
;;   :smoke — agents steer toward their own colour => coloured NETWORKS (slime)
;;   :haze  — agents don't steer, they wander => diffuse coloured SMOKE
;;   :trail — classic white transport network on a separate trail map
(def summer [[1.0 0.05 0.70] [1.0 0.55 0.0] [0.20 1.0 0.25]])  ; the summerfest hues (warm-skewed)
(def tropic [[1.0 0.05 0.70] [0.10 0.95 1.0] [1.0 0.55 0.0]])  ; summerfest with green->cyan: pink / cyan / orange
;; separated R/G/B primaries (like :jets) so the physarum colours stay distinct
;; instead of all skewing warm — gives vivid multi-colour smoke.
(def rgb3   [[1.0 0.12 0.10] [0.10 0.45 1.0] [0.20 1.0 0.28]])

(def themes
  {:jets    {:mode :sources
             :sources [{:color [1.0 0.15 0.10] :emit 1.0 :r 3 :motion {:type :brownian :base [0.35 0.55] :amp 0.0012}}
                       {:color [0.10 0.45 1.0] :emit 1.0 :r 3 :motion {:type :brownian :base [0.65 0.55] :amp 0.0012}}
                       {:color [0.20 1.0 0.30] :emit 0.8 :r 2 :motion {:type :brownian :base [0.50 0.50] :amp 0.0014}}]}
   :jet1    {:mode :sources   ; a SINGLE moving source; colour taken live from (:jet-color p)
             :sources [{:color [1.0 0.30 0.08] :emit 1.0 :r 3
                        :motion {:type :brownian :base [0.5 0.55] :amp 0.0035}}]}
   ;; --- physarum-driven smoke, variations ---------------------------------
   :slime   {:mode :smoke    ; agents steer toward own colour => coloured networks
             :palette rgb3
             :p-defaults {:p-count 3000 :p-sensor 9.0 :p-sense-angle 0.5 :p-turn 0.45
                          :p-speed 1.2 :p-deposit 0.28 :p-wind 0.2 :p-wander 0.0
                          :buoy 0.4 :keep 0.98 :expos 0.9 :saturation 3.5}}
   :haze    {:mode :haze     ; the smoke ITSELF is wandering agents => diffuse smoke, no networks
             :palette rgb3
             :p-defaults {:p-count 6000 :p-speed 1.0 :p-deposit 0.14 :p-wind 0.8 :p-wander 0.7
                          :buoy 0.5 :keep 0.96 :expos 1.1 :saturation 3.0}}
   :swarm   {:mode :haze     ; smoke SOURCES that are agents: few, bright, wandering emitters
             :palette rgb3
             :p-defaults {:p-count 600 :p-speed 1.8 :p-deposit 0.6 :p-wind 1.2 :p-wander 0.4
                          :buoy 0.7 :keep 0.96 :expos 1.1 :saturation 2.8}}
   :rivers  {:mode :smoke    ; faint steering + wander => flowing strands between network and smoke
             :palette rgb3
             :p-defaults {:p-count 4000 :p-sensor 16.0 :p-sense-angle 0.6 :p-turn 0.12
                          :p-speed 1.3 :p-deposit 0.22 :p-wind 0.9 :p-wander 0.25
                          :buoy 0.5 :keep 0.96 :expos 1.0 :saturation 3.2}}
   :network {:mode :trail
             :palette [[1.0 1.0 1.0]]
             :p-defaults {:p-count 3000 :p-sensor 9.0 :p-sense-angle 0.5 :p-turn 0.45
                          :p-speed 1.2 :p-deposit 0.2 :p-decay 0.90 :p-bright 0.6 :p-wander 0.0}}
   ;; beat-driven puffs: on each onset a single coloured smoke puff is stamped,
   ;; its angle/colour DETERMINISTIC from the spectrum (smoke.audio pushes the
   ;; spec into scene/audio-puffs). Mode :puffs runs NO physarum — only the puffs
   ;; (emit-puffs! in advance) + wind. Pure black background, the puffs are it.
   :puffs   {:mode :puffs
             :palette [[1.0 1.0 1.0]]
             :p-defaults {;; SLIME flow network: agents sense + deposit their own COLOUR
                          ;; (coloured networks, no blob) AND gently stir the fluid so the
                          ;; smoke flows down their coloured traces
                          :p-flow? true :p-flow 5.0 :p-flow-blend 0.4 :p-flow-paint 0.0
                          :p-freq-react? true :p-freq-speed 2.0 :p-freq-deposit 2.5
                          :p-count 2500 :p-sensor 9.0 :p-sense-angle 0.5 :p-turn 0.45
                          :p-speed 1.2 :p-deposit 0.28 :p-decay 0.90 :p-wind 0.0 :p-wander 0.2
                          :buoy 0.3 :keep 0.92 :expos 0.95 :saturation 3.2 :wind 2.0
                          :audio-puffs? true :audio-bands 10 :audio-dt-amp 0.09
                          :puff-radius 7.0 :puff-amount 2.6 :puff-vel 7.0 :puff-spawn-r 0.28
                          :puff-continuous? true :puff-spectral-scale 0.26 :puff-gain-gamma 0.5
                          :audio-wind-amp 2.0 :audio-wind-energy 0.6
                          :pulse? true :pulse-amount 1.0 :pulse-radius 0.3  ; near-single-cell point => filament smoke
                          :pulse-color [1.0 0.85 0.4] :pulse-random? true
                          :pulse-agents? true :pulse-agent-count 90 :pulse-agent-life 70
                          :pulse-ring 7.0 :pulse-bloom 0.5
                          :audio-sat 0.85 :audio-lift 0.1 :audio-palette-set nil
                          :audio-color-cycle 64}}})  ; random hues, flipped every 64 beats; pick a set in controls

(defn theme-defaults
  "Per-theme parameter overrides (merged into params on theme switch); nil if none."
  [theme-kw]
  (:p-defaults (get themes theme-kw)))

;; named agent-colour palettes for the physarum themes (controls "palette" dropdown)
(def palettes
  [[:rgb        rgb3]
   [:summerfest summer]
   [:fire       [[1.0 0.12 0.0] [1.0 0.45 0.0] [1.0 0.8 0.15]]]
   [:ice        [[0.1 0.45 1.0] [0.2 0.8 1.0] [0.75 0.95 1.0]]]
   [:neon       [[1.0 0.0 0.75] [0.0 1.0 0.85] [0.85 1.0 0.0]]]
   [:tropic     tropic]
   [:rainbow    [[1.0 0.1 0.1] [1.0 0.55 0.0] [0.9 1.0 0.0] [0.1 0.9 0.3] [0.1 0.5 1.0] [0.6 0.2 1.0]]]
   [:mono       [[1.0 1.0 1.0]]]])

;; curated multi-hue colour SETS for the audio look (puffs + slime), ordered as
;; gradients so bass->treble sweeps the ramp. Chosen to NOT be pure neon — warm,
;; earthy, icy, pastel moods. Pick via (:audio-palette-set p); nil => generated
;; random vivid hues (toned by :audio-sat / :audio-lift). See reroll-colors!.
(def audio-palettes
  {:sunset [[0.25 0.12 0.35] [0.6 0.15 0.4] [0.9 0.25 0.3] [1.0 0.45 0.2] [1.0 0.7 0.3] [1.0 0.9 0.6]]
   :ember  [[0.3 0.05 0.05] [0.6 0.12 0.08] [0.9 0.25 0.1] [1.0 0.45 0.12] [1.0 0.7 0.25] [1.0 0.9 0.5]]
   :forest [[0.05 0.2 0.12] [0.12 0.35 0.18] [0.2 0.5 0.25] [0.4 0.65 0.3] [0.3 0.6 0.5] [0.7 0.85 0.6]]
   :ice    [[0.05 0.1 0.3] [0.1 0.3 0.6] [0.2 0.55 0.85] [0.4 0.75 0.95] [0.7 0.9 1.0] [0.9 0.97 1.0]]
   :ocean  [[0.02 0.15 0.25] [0.05 0.3 0.4] [0.1 0.5 0.55] [0.15 0.65 0.6] [0.4 0.8 0.7] [0.8 0.95 0.85]]
   :sepia  [[0.18 0.12 0.08] [0.35 0.22 0.12] [0.55 0.38 0.2] [0.72 0.55 0.32] [0.85 0.72 0.5] [0.95 0.9 0.78]]
   :pastel [[0.95 0.7 0.75] [0.98 0.85 0.7] [0.95 0.95 0.75] [0.75 0.9 0.8] [0.75 0.85 0.95] [0.85 0.78 0.95]]
   :candy  [[0.95 0.4 0.6] [1.0 0.6 0.5] [0.8 0.4 0.8] [0.5 0.5 0.9] [0.4 0.8 0.85] [0.95 0.8 0.4]]
   :autumn [[0.3 0.1 0.05] [0.6 0.2 0.08] [0.85 0.4 0.12] [0.9 0.6 0.2] [0.7 0.65 0.25] [0.5 0.55 0.2]]})

;; named full-look presets (theme + params) for the controls "preset" dropdown.
;; :galaxy-slime is the saved hero look (vivid slime, denser keep).
(def presets
  [[:galaxy-slime (merge (theme-defaults :slime)   {:theme :slime   :keep 0.98 :palette rgb3})]
   [:summer-slime (merge (theme-defaults :slime)   {:theme :slime   :keep 0.98 :palette summer})]
   [:haze-smoke   (merge (theme-defaults :haze)    {:theme :haze    :palette rgb3})]
   [:summer-haze  (merge (theme-defaults :haze)    {:theme :haze    :palette summer})]
   [:rivers       (merge (theme-defaults :rivers)  {:theme :rivers  :palette rgb3})]
   [:tropic-rivers (merge (theme-defaults :rivers) {:theme :rivers  :palette tropic})]
   [:swarm        (merge (theme-defaults :swarm)   {:theme :swarm   :palette rgb3})]
   [:puffs        (merge (theme-defaults :puffs)   {:theme :puffs   :palette [[1.0 1.0 1.0]]})]
   [:white-net    (merge (theme-defaults :network) {:theme :network :keep 0.99 :expos 1.4 :saturation 1.0 :palette nil})]
   [:jets         {:theme :jets :keep 0.995 :expos 1.4 :saturation 1.0 :palette nil}]])

(defn preset-params
  "The params override map for a named preset (nil if unknown)."
  [preset-kw]
  (some (fn [[k v]] (when (= k preset-kw) v)) presets))

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
   :keep        0.98     ; density kept per frame (<1 => soft fade); higher = denser/more persistent
   :edge-margin 1        ; sponge-border width (cells); fades flow at edges (walls)
   :blur-passes 0        ; render-only density blur (0 = crisp)
   :expos       0.9      ; tonemap exposure per colour channel (lower = keeps colour, less white-out)
   :saturation  3.5      ; render chroma boost (push channels from grey mean); >1 = more vivid
   :wind        4.0      ; wind strength (noise flow-field force on the smoke)
   :noise-scale 2.0      ; wind spatial frequency
   :noise-speed 0.012    ; how fast the wind field evolves
   :theme       :slime   ; one of (keys themes)
   :palette     nil      ; agent colour palette override (nil => the theme's own); see scene/palettes
   :jet-color   [1.0 0.30 0.08]  ; live colour of the single source in the :jet1 theme
   :jet-count   3        ; number of moving sources in the :jets theme; extras get random palette colours
   :audio-amp   0.035    ; how much an audio band's energy raises its colour's keep (smoke.audio)
   :audio-dt-amp 0.15    ; extra dt kicked in on each beat onset (smoke.audio); base :dt is ~0.04
   :audio-lead-secs 0.0  ; render look-ahead (s): fire modulation early so the smoke bloom peaks ON the beat
   :audio-floor 0.035    ; in audio mode, how far below :keep silence drops a colour (smaller => denser quiet smoke)
   ;; beat/onset detection thresholds (smoke.audio): a beat = spectral-flux rising
   ;; edge above :beat-thresh (after the :beat-gate), re-armed once it drops below
   ;; :beat-rearm. Higher thresh/gate => only strong kicks count; lower => more onsets.
   :beat-gate   0.15     ; ignore flux below this, then rescale to 0..1
   :beat-thresh 0.30     ; rising edge above this fires a beat
   :beat-rearm  0.12     ; flux must fall below this before the next beat can fire
   :audio-beat-every 4   ; accent every Nth detected beat (downbeat)
   :audio-beat-accent 3.0 ; dt-kick multiplier on the accented (Nth) beat
   :audio-beat-base 2.0  ; dt-kick multiplier on every other beat (between normal=1 and accent)
   :audio-emit-amp 0.9   ; loudness -> extra emission: deposit scales by (1 + this * energy) (smoke.audio)
   :audio-emit-floor 0.45 ; fraction of the emission boost applied even in silence => smoke keeps flowing when quiet (smoke.audio)
   :audio-white? false   ; audio mode: agents deposit WHITE, each freq band fades in its palette colour
   :audio-white-density 0.5 ; deposit scale in :audio-white? mode (white fills all 3 channels => dimmer)
   :audio-agents? false  ; audio mode: agents keep their colour; each band ADDS intensity to its group
   :audio-agent-amp 1.5  ; band energy -> extra deposit: colour-group deposit × (1 + this × energy)
                         ; (at silence => exactly normal slime; loud band => that colour blooms)
   ;; --- beat puffs (:puffs theme): one coloured smoke puff per onset, angle +
   ;; colour deterministic from the spectrum; smoke.audio fills scene/audio-puffs ---
   :audio-puffs? false   ; emit a beat puff on each onset (spec computed in smoke.audio)
   :puff-radius  7.0     ; base puff blob radius (cells); grows with beat strength
   :puff-amount  2.6     ; peak density deposited per puff; grows with beat strength
   :puff-vel     7.0     ; outward velocity impulse along the spectral "clock" angle
   :puff-spawn-r 0.05    ; spawn radius from centre (normalized 0..1) along that angle
   :puff-angle   0.0     ; rotation offset of the spectral clock (rad)
   :puff-thresh  0.18    ; onset strength (0..1) above which a puff fires; LOWER => more puffs
                         ; (own trigger, independent of the dt-accent beat threshold)
   :puff-continuous? false ; NO threshold: every band puffs each frame from its gain
                         ; (the spectrum itself makes the puffs; ignores :puff-thresh)
   :puff-spectral-scale 0.15 ; per-frame deposit scale in continuous mode (it runs every frame)
   :puff-gain-gamma 1.0  ; continuous mode: amount = gain^this. <1 lifts QUIET tones (more smoke when soft)
   ;; --- audio-driven wind: beats gust the wind, loudness sustains it (smoke.audio) ---
   :audio-wind-amp    0.0 ; beat-gust: wind ×(1 + this × beat-kick)  => punchy surges on onsets
   :audio-wind-energy 0.0 ; loudness: wind ×(1 + this × energy)      => sustained breeze when loud
   ;; --- pulse glow: heuristic vocal-presence (mid/formant-band dominance) deposits
   ;; a WHITE glow in the centre (smoke.audio sets scene/audio-pulse) ---
   :pulse?       false   ; detect vocal-band energy and bloom a white centre glow
   :pulse-amount 0.12    ; per-frame white deposit at full pulse (small; it accumulates)
   :pulse-radius 10.0    ; centre-glow radius (cells)
   :pulse-thresh 0.12    ; pulse score (0..1) deadzone below which nothing shows
   :pulse-band-lo 0.2    ; vocal region start, as a FRACTION of the spectrum (bass below)
   :pulse-band-hi 0.65   ; vocal region end (treble above); the formant band sits between
   :pulse-contrast 0.6   ; how much bass+treble around it suppress the score (isolates midrange)
   ;; what a VOCAL ONSET spawns at the centre (instead of just the white glow):
   :pulse-agents?    false ; spawn a burst of SHORT-LIVED slime agents => a network blooms + fades
   :pulse-agent-count 90 ; agents per vocal-onset burst (scaled by onset strength)
   :pulse-agent-life 70  ; frames each transient agent lives
   :pulse-agent-deposit 0.5 ; bright deposit per agent (fades as it ages)
   :pulse-agent-speed 1.3
   :pulse-color  [1.0 0.85 0.4] ; colour of the pulse agents/ring (warm gold; NOT white)
   :pulse-shape  :point  ; pulse source shape: :point (soft gaussian, pulse-driven) | :circle |
                          ; :square | :rect | :triangle | :freq (dominant band picks the shape).
                          ; Non-point shapes draw only the OUTLINE, stepped a bit larger EVERY
                          ; FRAME from min..max, then wrap back to min with a fresh shape.
   :pulse-shape-size 90  ; max half-size/radius (cells) the outline grows to before wrapping
   :pulse-shape-min  3   ; starting half-size after each wrap
   :pulse-shape-steps 5  ; number of discrete size steps from min..max before a fresh shape
   :pulse-shape-every 4  ; beats each MIDDLE growth step stays before the next step
   :pulse-shape-edge-beats 16 ; beats the SMALLEST and LARGEST step each stay (longer dwell at the ends)
   :pulse-shape-rest-beats 32 ; beats with NO shape between cycles (quiet gap after each growth)
   :pulse-rotate     0.0 ; shape rotation (rad/frame); 0 = no spin
   :pulse-rect-aspect 1.7 ; how much :rect is wider than tall
   :pulse-line-width 2   ; outline thickness (cells) of the shape
   :pulse-random? false  ; place the pulse glow at a random cell each frame (scattered shimmer)
   :pulse-ring   0.0     ; expanding RING impulse on each vocal onset (0 => off)
   :pulse-bloom  0.0     ; whole-image exposure BLOOM ∝ pulse score (0 => off; the scene breathes)
   ;; --- audio palette MOOD: tone the generated vivid hues so it's not only neon ---
   :audio-sat    1.0     ; chroma of the audio palette: 1 = full neon, lower => muted/greyed
   :audio-lift   0.0     ; blend the palette toward WHITE: 0 = none, higher => pastel/washed
   :audio-palette-set nil ; a curated colour set from scene/audio-palettes (e.g. :sunset :ice :sepia),
                          ; or nil => freshly generated random vivid hues. Re-roll ('r') to apply.
   :audio-color-cycle 0  ; re-roll the palette every Nth detected beat (0 = off); best in random mode
                          ; => the puffs AND slime network shift to a fresh colour set on the beat
   :audio-colors  7      ; # of random agent hues in audio mode (decoupled from band count, which
                         ; stays = palette size for the keep-modulation). More => more colour variety.
   ;; --- depth: nested zoomed + dimmer copies behind the main frame (a zoom tunnel) ---
   :depth-layer false    ; draw the receding back layers for a sense of depth (off by default)
   :depth-layers 3       ; how many back copies (each larger & dimmer => recedes)
   :depth-scale 0.35     ; extra zoom per layer (all > window => edges off-screen, no square)
   :depth-dim   0.5      ; brightness falloff per layer (lower = darker behind, like less exposure)
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
   :p-flow?       false  ; INVISIBLE flow network: agents deposit no colour, they stir the fluid
                         ; along their slime traces so the smoke flows down them (works any theme)
   :p-freq-react? false  ; split agents into one BUCKET per freq band; each band's gain boosts
                         ; that bucket's speed/deposit (a spectral alternative to the pulse shape)
   :p-freq-speed  0.0    ; band gain -> bucket SPEED: speed ×(1 + this × gain)
   :p-freq-deposit 0.0   ; band gain -> bucket DEPOSIT: deposit ×(1 + this × gain)
   :p-flow        12.0   ; TARGET flow speed (cells/tick) :flow agents drive the fluid toward
   :p-flow-blend  0.4    ; blend rate toward that target; BOUNDS u/v so the flow can't blow up
   :p-flow-paint  0.0    ; :flow agents also paint WHITE this much => visible white network (0 => invisible)
   :p-rand-color? false  ; colour agents from a freshly generated random vivid palette (ignores :palette);
                         ; a new colour set is rolled on each field rebuild ('r'). Re-seed to apply.
   :p-rand-colors 6      ; how many random hues in that palette (=> that many coherent colour networks)
   :boids         nil})  ;; boids config (:sources mode); nil => boids/default-boid

(defn theme [p] (get themes (:theme p) (:jets themes)))
(defn mode  [p] (:mode (theme p)))

(defonce frame   (atom 0))    ; frame counter, drives motion + wind time
(defonce src-pos (atom nil))  ; {:theme kw :pos [...] :vel [...]} — live source state
(defonce audio-keep (atom nil)) ; [kr kg kb] per-channel keep set by smoke.audio; nil => use scalar :keep
(defonce audio-dt   (atom nil)) ; transient dt boost on beats, set by smoke.audio; nil/0 => none
(defonce audio-emit (atom nil)) ; emission (deposit) multiplier from loudness, set by smoke.audio; nil/0 => none
(defonce audio-wind (atom nil)) ; wind-strength boost from beats/loudness, set by smoke.audio; nil/0 => steady wind
(defonce audio-pulse (atom nil)) ; vocal-presence score (0..1) set by smoke.audio; drives the pulse effects
(defonce pulse-agents (atom [])) ; short-lived slime agents spawned at vocal onsets {:x :y :h :life}
(defonce ^:private pulse-prev (atom 0.0)) ; previous pulse score, for rising-edge onset detection
(defonce recolor-pending? (atom false)) ; smoke.audio sets this on a colour-cycle beat; advance recolours the agents
(defonce beat-count (atom 0))   ; running detected-beat counter mirrored from smoke.audio (drives shape growth)
(defonce ^:private pulse-cyc (atom {:gstep -1 :shape :circle :ang 0.0 :rot 0.0})) ; beat-stepped growing-shape state
(def pulse-bright-colors        ; bright pulse-source colours flipped to on the colour-cycle beat
  [[1.0 1.0 1.0] [1.0 0.85 0.4] [1.0 0.35 0.35] [0.35 1.0 0.45] [0.35 0.6 1.0]
   [1.0 0.95 0.35] [1.0 0.45 1.0] [0.4 1.0 1.0] [1.0 0.6 0.2]])
(defonce pulse-color-cur (atom nil)) ; current random bright pulse colour (smoke.audio flips it); nil => :pulse-color
(defonce audio-gains (atom nil)) ; per-band gains [g0 g1 ..] for :audio-white? mode (agents fade white->colour)
(defonce audio-palette (atom nil)) ; generated vivid-hue palette set by smoke.audio ('r' re-rolls it); nil => theme palette
(defonce audio-puffs (atom []))   ; pending beat-puff specs from smoke.audio; drained once per frame in `advance`
(defonce audio-hook (atom nil)) ; 0-arg fn run once per sim frame (smoke.audio drives keep/dt from playback)
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
  (let [n (long (:n fl))
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

(defn- build-jet-srcs
  "Source list for the :jets theme sized to (:jet-count p). The first sources are
   the theme's fixed RGB jets; any extras are spawned at random positions with a
   random colour drawn from the active palette (`:palette p`, else rgb3)."
  [p]
  (let [base (vec (:sources (:jets themes)))
        n    (long (max 1 (long (:jet-count p (count base)))))
        pal  (vec (or (seq (:palette p)) rgb3))]
    (if (<= n (count base))
      (subvec base 0 n)
      (into base
            (repeatedly (- n (count base))
                        #(hash-map :color (rand-nth pal) :emit 1.0 :r 3
                                   :motion {:type :brownian
                                            :base [(+ 0.2 (* 0.6 (double (rand)))) (+ 0.2 (* 0.6 (double (rand))))]
                                            :amp 0.0012}))))))

(defn- emit-jets! [fl p t]
  ;; :jets sizes its source list to (:jet-count p); jet1 keeps its single source.
  (let [reseed? (or (nil? (:vel @src-pos))
                    (not= (:theme p) (:theme @src-pos))
                    (and (= (:theme p) :jets)
                         (not= (long (:jet-count p 3)) (count (:srcs @src-pos)))))]
    (when reseed?
      (let [srcs (if (= (:theme p) :jets) (build-jet-srcs p) (:sources (theme p)))]
        (reset! src-pos {:theme (:theme p)
                         :srcs srcs
                         :pos (mapv (comp :base :motion) srcs)
                         :vel (mapv (constantly [0.0 0.0]) srcs)})))
    (let [srcs      (:srcs @src-pos)
          accels    (boids/accelerations (:pos @src-pos) (:vel @src-pos) (:boids p))
          steps     (mapv #(step-source %1 %2 %3 %4 t) srcs (:pos @src-pos) (:vel @src-pos) accels)
          positions (mapv first steps)]
      (swap! src-pos assoc :pos positions :vel (mapv second steps))
      ;; :jet1 = single source whose colour is steered live via (:jet-color p)
      (let [srcs (if (and (= (:theme p) :jet1) (:jet-color p))
                   (mapv #(assoc % :color (:jet-color p)) srcs)
                   srcs)]
        (emit-sources! fl srcs positions)))))

;; ---- noise flow-field wind -------------------------------------------------
(defn- apply-wind! [fl p ^double t]
  (let [n (long (:n fl))
        ^floats fx (:fx fl) ^floats fy (:fy fl) ^floats d (:dens fl)
        ;; audio layer can surge the wind on beats / with loudness (smoke.audio)
        w (* (double (:wind p)) (+ 1.0 (double (or @audio-wind 0.0))))
        sc (* TAU (double (:noise-scale p)))
        sp (* (double (:noise-speed p)) t) inv (/ 1.0 (double n))]
    (when (pos? w)
      (f/par-rows
       n
       (fn [^long j]
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
                   (aset fy k (float (+ (aget fy k) (* w wy dk))))))))))))))

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
  "Fresh state: fluid grid + Physarum agents coloured from the theme palette,
   or from the live audio's seed-generated hue palette when one is set ('r')."
  [p]
  (reset! stars [])
  (reset! pulse-agents []) (reset! pulse-prev 0.0) (reset! recolor-pending? false)
  (reset! pulse-cyc {:gstep -1 :shape :circle :ang 0.0 :rot 0.0}) (reset! pulse-color-cur nil)
  (let [n   (grid-n p)
        ;; :p-rand-color? => a freshly generated random vivid palette (new set each
        ;; rebuild, i.e. each 'r'); else audio's hue palette / the theme's own.
        pal (cond
              ;; freshly generated random vivid palette (new set each rebuild / 'r')
              (:p-rand-color? p) (phys/rand-vivid-palette (:p-rand-colors p 6))
              ;; :flow (slime) agents => COLOURED networks: prefer the live vivid hue
              ;; palette (matches the puff colours), else rgb3. Several hues => the
              ;; slime forms separate coloured networks instead of blobbing.
              (:p-flow? p)       (or @audio-palette (when (seq (:palette p)) (when (> (count (:palette p)) 1) (:palette p))) rgb3)
              ;; other :audio-puffs? looks: white agents (colour lives in the puffs)
              (:audio-puffs? p)  (or (:palette p) (:palette (theme p)) [[1.0 1.0 1.0]])
              :else (or @audio-palette (:palette p) (:palette (theme p)) [[1.0 1.0 1.0]]))]
    (assoc (f/make-fluid n)
           :phys (phys/make n (:p-count p) pal))))

(defn emit-puffs!
  "Drain and stamp any pending beat puffs into the fluid. Each puff (from
   smoke.audio, with position + colour derived from the spectrum) is a Gaussian
   density blob plus a ONE-SHOT directional velocity impulse over the same
   footprint, so it blooms and shoots outward along its spectral angle. The
   velocity is added straight to :u/:v just before the velocity step advects +
   projects it. No-op (cheap) when nothing is pending."
  [fl]
  (let [puffs @audio-puffs]
    (when (seq puffs)
      (reset! audio-puffs [])
      (let [n (long (:n fl))
            ^floats dr (:dr fl) ^floats dg (:dg fl) ^floats db (:db fl)
            ^floats u  (:u fl)  ^floats v  (:v fl)]
        (doseq [puff puffs]
          (let [ci  (long (* (double (:x puff)) n)) cj (long (* (double (:y puff)) n))
                sig (max 1.0 (double (:r puff)))
                inv (/ 1.0 (* 2.0 sig sig))
                amt (double (:amount puff))
                col (:color puff)
                cr  (double (nth col 0)) cg (double (nth col 1)) cb (double (nth col 2))
                vx  (double (:vx puff)) vy (double (:vy puff))
                ri  (long (Math/ceil (* 2.5 sig)))]
            (dotimes [a (inc (* 2 ri))]
              (let [oj (- a ri) jj (long (mod (+ cj oj) n))]
                (dotimes [bcol (inc (* 2 ri))]
                  (let [oi (- bcol ri) ii (long (mod (+ ci oi) n))
                        d2 (double (+ (* oi oi) (* oj oj)))
                        g  (Math/exp (- (* d2 inv)))
                        k  (f/idx n ii jj)]
                    (aset dr k (float (+ (aget dr k) (* amt g cr))))
                    (aset dg k (float (+ (aget dg k) (* amt g cg))))
                    (aset db k (float (+ (aget db k) (* amt g cb))))
                    (aset u  k (float (+ (aget u k)  (* vx g))))
                    (aset v  k (float (+ (aget v k)  (* vy g))))))))))))))

(def ^:const PULSE-AGENT-MAX 1500)   ; cap on live transient pulse agents (perf guard)

(defn- stamp-shape-outline!
  "Draw the ROTATED OUTLINE (band of width `lw`) of `shape` centred at (ci,cj):
   :circle / :square / :rect / :triangle. `r` = radius / box half-size (circum-
   radius for the triangle); `aspect` widens :rect in x; `ang` rotates the shape.
   Deposits `amt` of (cr cg cb). Clamped to the grid (no periodic wrap)."
  [fl ci cj shape r aspect lw ang amt cr cg cb]
  (let [n (long (:n fl))
        ^floats dr (:dr fl) ^floats dg (:dg fl) ^floats db (:db fl)
        ci (long ci) cj (long cj)
        r (double r) aspect (double aspect) lw (double lw) ang (double ang)
        amt (double amt) cr (double cr) cg (double cg) cb (double cb)
        hl (* 0.5 lw)
        ca (Math/cos ang) sa (Math/sin ang)
        hw (* r aspect) hh r                          ; box half-extents (:rect widens x)
        ext (long (Math/ceil (+ lw (Math/sqrt (+ (* hw hw) (* hh hh))))))  ; safe bounding box
        rin (- r hl) rout (+ r hl) r2in (* rin rin) r2out (* rout rout)
        ;; triangle edge normals (3), line at inradius = r/2 from centre, rotated by ang
        a0 (+ ang (/ (* 5.0 Math/PI) 6.0)) a1 (+ a0 2.0943951) a2 (+ a1 2.0943951)
        n0c (Math/cos a0) n0s (Math/sin a0) n1c (Math/cos a1) n1s (Math/sin a1)
        n2c (Math/cos a2) n2s (Math/sin a2) toff (* 0.5 r)]
    (loop [oj (- ext)]
      (when (<= oj ext)
        (let [jj (+ cj oj)]
          (when (and (>= jj 0) (< jj n))
            (loop [oi (- ext)]
              (when (<= oi ext)
                (let [ii (+ ci oi)]
                  (when (and (>= ii 0) (< ii n))
                    (let [x (double oi) y (double oj)
                          on? (case shape
                                :circle (let [d2 (+ (* x x) (* y y))] (and (>= d2 r2in) (<= d2 r2out)))
                                :triangle (let [d0 (- (+ (* x n0c) (* y n0s)) toff)
                                                d1 (- (+ (* x n1c) (* y n1s)) toff)
                                                d2 (- (+ (* x n2c) (* y n2s)) toff)
                                                m  (max d0 (max d1 d2))]
                                            (<= (if (neg? m) (- m) m) hl))
                                ;; :square / :rect — rotate point into shape frame, box-border test
                                (let [lx (+ (* x ca) (* y sa)) ly (+ (* (- x) sa) (* y ca))
                                      ax (if (neg? lx) (- lx) lx) ay (if (neg? ly) (- ly) ly)]
                                  (and (<= ax (+ hw hl)) (<= ay (+ hh hl))
                                       (or (>= ax (- hw hl)) (>= ay (- hh hl))))))]
                      (when on?
                        (let [k (f/idx n ii jj)]
                          (aset dr k (float (+ (aget dr k) (* amt cr))))
                          (aset dg k (float (+ (aget dg k) (* amt cg))))
                          (aset db k (float (+ (aget db k) (* amt cb)))))))))
                (recur (inc oi))))))
        (recur (inc oj))))))

(defn emit-pulse!
  "Pulse-driven centre effects, each an INDEPENDENT toggle, all scaled by the
   vocal-presence score @audio-pulse (deadzone :pulse-thresh), coloured :pulse-color:
     :pulse-amount  — a continuous glow at the centre (radius :pulse-radius; tiny
                      radius ~1 => a near-point source the fluid smears into filaments).
     :pulse-ring    — an expanding ring (puffs round a circle, outward velocity) on each
                      vocal ONSET (rising edge).
     :pulse-agents? — a burst of SHORT-LIVED slime agents on each onset => a network
                      blooms from the centre then fades.
     (:pulse-bloom is applied in render-pixels! — whole-image exposure pulse.)"
  [fl p]
  (let [v   (double (or @audio-pulse 0.0))
        thr (double (:pulse-thresh p 0.12))
        over (- v thr)
        n   (long (:n fl))
        col (or @pulse-color-cur (:pulse-color p [1.0 0.85 0.4]))  ; random bright colour flips it
        cr (double (nth col 0)) cg (double (nth col 1)) cb (double (nth col 2))
        ^floats dr (:dr fl) ^floats dg (:dg fl) ^floats db (:db fl)
        ci (quot n 2) cj (quot n 2)]
    ;; (1) continuous glow in pulse-colour (radius small => point source). With
    ;; :pulse-random? the point jumps to a fresh random cell each frame (scattered
    ;; pulse shimmer) instead of sitting at the centre.
    (when (pos? (double (:pulse-amount p 0.0)))
      (let [shape (:pulse-shape p :point)
            gi (if (:pulse-random? p) (long (rand n)) ci)
            gj (if (:pulse-random? p) (long (rand n)) cj)]
        (if (= shape :point)
          ;; soft gaussian point — pulse-score driven (only when a pulse is present)
          (when (pos? over)
            (let [amt0 (* (double (:pulse-amount p)) over)
                  sig (max 0.2 (double (:pulse-radius p 10.0)))
                  inv (/ 1.0 (* 2.0 sig sig))
                  ri (long (Math/ceil (* 2.5 sig)))]
              (dotimes [a (inc (* 2 ri))]
                (let [oj (- a ri) jj (long (mod (+ gj oj) n))]
                  (dotimes [bcol (inc (* 2 ri))]
                    (let [oi (- bcol ri) ii (long (mod (+ gi oi) n))
                          d2 (double (+ (* oi oi) (* oj oj)))
                          g (* amt0 (Math/exp (- (* d2 inv))))
                          k (f/idx n ii jj)]
                      (aset dr k (float (+ (aget dr k) (* g cr))))
                      (aset dg k (float (+ (aget dg k) (* g cg))))
                      (aset db k (float (+ (aget db k) (* g cb))))))))))
          ;; geometric SHAPE outline, beat-synced: a cycle = GROW (steps × every
          ;; beats, jumping one size step that STAYS every :pulse-shape-every beats)
          ;; then a REST of :pulse-shape-rest-beats beats with NO shape, then a
          ;; fresh shape. The dominant FREQ band picks the shape; rotation applies
          ;; ONLY to triangles (bass third => spin one way, treble => the other).
          (let [cyc   @pulse-cyc
                every (long (max 1 (:pulse-shape-every p 4)))
                edge  (long (max 1 (:pulse-shape-edge-beats p 16)))
                steps (long (max 1 (:pulse-shape-steps p 5)))
                rest  (long (max 0 (:pulse-shape-rest-beats p 16)))
                ;; per-step dwell: edge beats for the smallest & largest, every for middle
                step-dur (fn [s] (if (or (zero? s) (= s (dec steps))) edge every))
                grow  (reduce + (map step-dur (range steps)))
                cycle (max 1 (+ grow rest))
                rmin  (double (:pulse-shape-min p 3))
                rmax  (double (:pulse-shape-size p 90))
                bc    (long @beat-count)
                bphase (mod bc cycle)                  ; beat within the grow+rest cycle
                new-beat? (not= bc (long (:last-bc cyc -1)))
                wrap? (and new-beat? (zero? bphase))   ; start of a cycle => fresh shape
                ;; which step is bphase in (or rest)? walk the per-step durations
                gs    (loop [acc 0 i 0]
                        (if (>= i steps) [false (dec steps)]
                            (let [d (long (step-dur i))]
                              (if (< bphase (+ acc d)) [true i] (recur (+ acc d) (inc i))))))
                growing? (nth gs 0)
                s     (long (nth gs 1))
                ;; dominant-band fraction 0..1 (drives shape + triangle rotation on a wrap)
                ^doubles gains @audio-gains
                frac (if (and gains (pos? (alength gains)))
                       (let [nn (alength gains)
                             dom (loop [i 1 bi 0 bv (aget gains 0)]
                                   (if (< i nn)
                                     (if (> (aget gains i) bv) (recur (inc i) i (aget gains i)) (recur (inc i) bi bv))
                                     bi))]
                         (/ (double dom) (double (max 1 nn))))
                       0.5)
                fresh? (or wrap? (nil? (:shape cyc)))
                shp  (if fresh?
                       (if (= shape :freq)
                         (nth [:square :rect :triangle :circle] (min 3 (long (* frac 4.0))))
                         shape)
                       (:shape cyc))
                ;; rotation ONLY for triangles; direction from the freq band
                rot  (if fresh?
                       (if (= shp :triangle)
                         (* (double (:pulse-rotate p 0.0)) (if (< frac 0.5) -1.0 1.0))
                         0.0)
                       (double (:rot cyc 0.0)))
                r    (+ rmin (* (- rmax rmin) (/ (double s) (double (max 1 (dec steps))))))
                ang  (+ (double (:ang cyc 0.0)) rot)
                lw   (double (:pulse-line-width p 2))
                amt  (double (:pulse-amount p 1.0))
                aspect (double (:pulse-rect-aspect p 1.7))]
            (reset! pulse-cyc {:last-bc bc :shape shp :ang ang :rot rot})
            (when growing?
              (stamp-shape-outline! fl ci cj shp r aspect lw ang amt cr cg cb))))))
    ;; (2)+(3) rising-edge onset => expanding ring + short-lived agent burst
    (let [pv (double @pulse-prev)]
      (when (and (> v thr) (<= pv thr))
        (let [strength (min 1.0 v)]
          (when (pos? (double (:pulse-ring p 0.0)))
            (let [k 24 vel (* (double (:pulse-ring p)) (+ 0.5 (* 0.5 strength))) sr 0.04]
              (dotimes [i k]
                (let [ang (* TAU (/ (double i) k)) dx (Math/cos ang) dy (Math/sin ang)]
                  (swap! audio-puffs conj
                         {:x (+ 0.5 (* sr dx)) :y (+ 0.5 (* sr dy))
                          :color col :r 5.0 :amount 1.5 :vx (* vel dx) :vy (* vel dy)})))))
          (when (and (:pulse-agents? p) (< (count @pulse-agents) PULSE-AGENT-MAX))
            (let [nb (long (* (double (:pulse-agent-count p 90)) strength))
                  life (long (:pulse-agent-life p 70)) cx (double ci) cy (double cj)]
              (swap! pulse-agents into
                     (repeatedly nb (fn [] {:x cx :y cy :h (* TAU (double (rand))) :life life})))))))
      (reset! pulse-prev v))
    ;; step live pulse agents: slime-steer on the pulse colour, deposit (fading with
    ;; age), move, cull the dead
    (when (seq @pulse-agents)
      (let [so (double (:p-sensor p 9.0)) sa (double (:p-sense-angle p 0.5))
            ra (double (:p-turn p 0.45)) ss (double (:pulse-agent-speed p 1.3))
            dep (double (:pulse-agent-deposit p 0.5)) life0 (double (max 1 (long (:pulse-agent-life p 70))))
            sns (fn ^double [^double x ^double y]
                  (let [k (f/idx n (long (mod x n)) (long (mod y n)))]
                    (+ (* cr (aget dr k)) (* cg (aget dg k)) (* cb (aget db k)))))
            stepped (into [] (keep (fn [a]
                                     (let [x (double (:x a)) y (double (:y a)) h (double (:h a))
                                           cl  (sns (+ x (* so (Math/cos (- h sa)))) (+ y (* so (Math/sin (- h sa)))))
                                           cc  (sns (+ x (* so (Math/cos h)))        (+ y (* so (Math/sin h))))
                                           crr (sns (+ x (* so (Math/cos (+ h sa)))) (+ y (* so (Math/sin (+ h sa)))))
                                           h2 (cond (and (>= cc cl) (>= cc crr)) h
                                                    (and (< cc cl) (< cc crr)) (if (< (double (rand)) 0.5) (- h ra) (+ h ra))
                                                    (> cl crr) (- h ra) :else (+ h ra))
                                           nx (double (mod (+ x (* ss (Math/cos h2))) n))
                                           ny (double (mod (+ y (* ss (Math/sin h2))) n))
                                           life (dec (long (:life a)))]
                                       (when (pos? life)
                                         (let [k (f/idx n (long nx) (long ny)) f (* dep (/ (double life) life0))]
                                           (aset dr k (float (+ (aget dr k) (* f cr))))
                                           (aset dg k (float (+ (aget dg k) (* f cg))))
                                           (aset db k (float (+ (aget db k) (* f cb)))))
                                         {:x nx :y ny :h h2 :life life})))
                                   @pulse-agents))]
        (reset! pulse-agents stepped)))))

(defn advance
  "One tick: velocity → (Physarum emission for :slime/:network) → advect/dissipate
   → spawn/drift persistent stars."
  [fl p]
  (when-let [h @audio-hook] (h))  ; refresh audio-keep/audio-dt from playback, in lockstep with frames
  (emit-puffs! fl)                ; stamp beat puffs (density + impulse) before the velocity step
  (emit-pulse! fl p)              ; white centre glow while a pulse is present
  (let [dt (+ (double (:dt p)) (double (or @audio-dt 0.0)))  ; beat onsets kick dt up briefly
        fl (f/vel-step fl (:visc p) dt (:buoy p))
        m  (mode p)]
    ;; colour-cycle beat (smoke.audio rolled a fresh @audio-palette): repaint the
    ;; live agents to the new hues so the network shifts colour with the puffs
    (when @recolor-pending?
      (reset! recolor-pending? false)
      (when-let [pal @audio-palette]
        (when-let [ph (:phys fl)] (phys/recolor! ph pal))))
    (cond
      ;; invisible flow network: agents stir the fluid (no colour) so the smoke
      ;; rides their traces. Runs AFTER vel-step so advect-colors! below carries
      ;; the colour along the freshly-injected currents. Works under any theme.
      (:p-flow? p)
      (phys/step! (:phys fl) fl (assoc p :p-mode :flow :audio-gains @audio-gains))
      (#{:smoke :trail :haze} m)
      (let [em (double (or @audio-emit 0.0))                ; louder audio => more smoke deposited
            pp (assoc p :p-mode m
                      :p-deposit (* (double (:p-deposit p)) (+ 1.0 em))
                      :audio-gains @audio-gains)]           ; per-band gains for :audio-white? deposit
        (phys/step! (:phys fl) fl pp)))
    (let [fl (f/advect-colors! fl dt)
          ;; audio layer overrides scalar keep with a per-colour [kr kg kb]
          fl (if-let [ak @audio-keep]
               (f/dissipate-colors-rgb! fl (nth ak 0) (nth ak 1) (nth ak 2))
               (f/dissipate-colors! fl (:keep p)))
          fl (-> fl
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
  (let [n (long (:n fl))
        w (render-w p) h (render-h p) mx (max w h)
        gscale (/ (double (dec n)) (double (dec mx)))     ; grid units per output px (cover)
        offx (* 0.5 (- (double mx) (double w)))            ; centring offset (cropped axis)
        offy (* 0.5 (- (double mx) (double h)))
        ipx  (/ 1.0 gscale)                                ; grid -> output px factor
        ^floats dr (:dr fl) ^floats dg (:dg fl) ^floats db (:db fl)
        thresh (double (:star-thresh p))
        moved (into [] (keep (fn [s]
                               (let [vx (+ (double (:vx s)) (double (:ax s)))
                                     vy (+ (double (:vy s)) (double (:ay s)))
                                     nx (+ (double (:x s)) vx)
                                     ny (+ (double (:y s)) vy)]
                                 ;; die at the edge instead of clamping
                                 (when (and (> nx 0.0) (< nx (dec w)) (> ny 0.0) (< ny (dec h)))
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
              (let [px (- (* i ipx) offx) py (- (* j ipx) offy)]   ; grid -> visible output px
                (when (and (>= px 0.0) (< px (dec w)) (>= py 0.0) (< py (dec h))
                           (not-any? (fn [s]
                                       (let [ex (- px (double (:x s))) ey (- py (double (:y s)))]
                                         (< (+ (* ex ex) (* ey ey)) STAR-MINDIST2)))
                                     @acc))
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
  (let [w (render-w p) h (render-h p) R (long (:star-radius p)) speed (double (:star-speed p))]
    (doseq [s @stars]
      (let [cx (long (:x s)) cy (long (:y s))
            cr0 (double (:r s)) cg0 (double (:g s)) cb0 (double (:b s))
            flash (+ 0.5 (* 0.5 (Math/sin (+ (* t speed) (* (double (:x s)) 0.05)))))
            rr (+ cr0 (* (- 1.0 cr0) flash))
            gg (+ cg0 (* (- 1.0 cg0) flash))
            bbq (+ cb0 (* (- 1.0 cb0) flash))]
        (dotimes [dy (inc (* 2 R))]
          (let [oy (- dy R) py (+ cy oy)]
            (when (and (>= py 0) (< py h))
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
  (let [n      (long (:n fl))
        w      (render-w p)
        h      (render-h p)
        mx     (max w h)
        nm1    (dec n)
        ^floats br (:br fl) ^floats bg (:bg fl) ^floats bb (:bb fl)
        ^floats trail (:trail (:phys fl))
        netw   (double (if (= (mode p) :trail) (:p-bright p) 0.0))
        ;; :pulse-bloom => whole-image exposure pulse with the vocal-presence score
        ^floats tl (tone-lut (* (double (:expos p))
                                (+ 1.0 (* (double (:pulse-bloom p 0.0)) (double (or @audio-pulse 0.0))))))
        tscale (/ (double (dec TLUTN)) CHMAX)
        ;; saturation: amplify each pixel's chroma around its mean so the dominant
        ;; hue shows instead of washing to white. 1.0 = neutral; higher = vivid.
        ;; HUE-PRESERVING: we scale the chroma vector (cr-mean, cg-mean, cb-mean)
        ;; by `sat`, but never past the point where a channel would clip to 0 or
        ;; CHMAX — clipping a single channel snaps the colour to an RGB-cube corner
        ;; (red/green/blue), which flattens varied palettes. Capping to gamut keeps
        ;; oranges/teals/purples as themselves. (sat<=1 reduces to the identity.)
        sat    (double (:saturation p 1.0))
        cap    (double CHMAX)
        ;; cover-fit the square grid into the w x h frame: isotropic scale by the
        ;; long edge, centre the short edge (=> crop, never stretch). w==h reduces
        ;; to the old full-grid mapping.
        gscale (/ (double nm1) (double (dec mx)))
        offx   (* 0.5 (- (double mx) (double w)))
        offy   (* 0.5 (- (double mx) (double h)))]
    (f/par-rows
     h
     (fn [^long oy]
       (let [gy (* (+ oy offy) gscale) j0 (long gy) fy (- gy j0)
             j1 (min nm1 (inc j0)) sy0 (- 1.0 fy) row (* oy w)]
         (dotimes [ox w]
           (let [gx (* (+ ox offx) gscale) i0 (long gx) fx (- gx i0)
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
                 mean (* (/ 1.0 3.0) (+ cr cg cb))   ; push channels away from grey mean
                 dr (- cr mean) dg (- cg mean) db (- cb mean)
                 ;; largest chroma scale in [1,sat] keeping every channel in gamut
                 ;; (so chroma is boosted but never clipped to a cube corner). In
                 ;; over-bright overlaps it falls back to 1.0 => raw, which the
                 ;; tonemap blows out to white (the nice overlap glow is kept).
                 sr (if (> dr 1.0e-6) (/ (- cap mean) dr) (if (< dr -1.0e-6) (/ (- mean) dr) sat))
                 sg (if (> dg 1.0e-6) (/ (- cap mean) dg) (if (< dg -1.0e-6) (/ (- mean) dg) sat))
                 sb (if (> db 1.0e-6) (/ (- cap mean) db) (if (< db -1.0e-6) (/ (- mean) db) sat))
                 s  (max 1.0 (min sat sr sg sb))
                 ri (long (aget tl (min (dec TLUTN) (max 0 (long (* (+ mean (* s dr)) tscale))))))
                 gi (long (aget tl (min (dec TLUTN) (max 0 (long (* (+ mean (* s dg)) tscale))))))
                 bi (long (aget tl (min (dec TLUTN) (max 0 (long (* (+ mean (* s db)) tscale))))))]
             (aset px (+ ox row)
                   (unchecked-int (bit-or (unchecked-int 0xFF000000)
                                          (bit-shift-left ri 16) (bit-shift-left gi 8) bi))))))))
    (when (:stars p) (draw-stars! px p (double @frame)))
    px))
