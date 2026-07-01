(ns smoke.physarum
  "Physarum (slime-mould) layer — Jeff Jones, Artificial Life 2010. Two modes:

   :smoke  — agents emit the SMOKE. They sense the smoke DENSITY as the
             chemoattractant (density == trail; no separate trail map) and
             deposit their own COLOUR into the smoke channels; the fluid then
             advects it. The smoke self-organises into flowing coloured networks.

   :trail  — the classic look: a separate WHITE trail map. Agents sense the
             trail, deposit white into it, and the trail diffuses + decays. Renders
             as a white transport network (the smoke channels stay empty).

   In both modes agents are nudged by the fluid velocity (wind)."
  (:require [smoke.fluid :as f]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(def ^:const PI2 6.283185307179586)

(defn- hsv->rgb
  "[r g b] in 0..1 from hue/sat/val in 0..1."
  [h s v]
  (let [rgb (java.awt.Color/HSBtoRGB (float h) (float s) (float v))]
    [(/ (bit-and (bit-shift-right rgb 16) 0xff) 255.0)
     (/ (bit-and (bit-shift-right rgb 8) 0xff) 255.0)
     (/ (bit-and rgb 0xff) 255.0)]))

(defn vivid-palette*
  "`n` vivid hues drawn from RNG `rng`: a random BASE hue plus a random ARC SPAN
   across the wheel (+ small per-hue jitter). Because the span varies, each call
   has a visibly different CHARACTER — sometimes analogous/warm/cool, sometimes
   near-rainbow — instead of always landing on a (merely rotated) full wheel, which
   looked identical every reseed. Agents share these `n` colours so each forms its
   own coherent network (no speckle)."
  [^java.util.Random rng ^long n]
  (let [nn   (max 1 n)
        h0   (.nextDouble rng)
        span (+ 0.22 (* 0.66 (.nextDouble rng)))   ; arc covered: 0.22..0.88 of the wheel
        jit  0.05]
    (mapv (fn [i]
            (let [t (if (> nn 1) (/ (double i) (double (dec nn))) 0.0)
                  h (mod (+ h0 (* span t) (* jit (- (.nextDouble rng) 0.5))) 1.0)]
              (hsv->rgb h 1.0 1.0)))
          (range nn))))

(defn rand-vivid-palette
  "A fresh random vivid palette each call (see `vivid-palette*`). Used by the
   `:p-rand-color?` look."
  [^long n]
  (vivid-palette* (java.util.Random.) n))

(defn make
  "Allocate `cnt` agents (random pos/heading), coloured from `palette`, plus a
   trail map (used only in :trail mode)."
  [^long n ^long cnt palette]
  (let [xs (float-array cnt) ys (float-array cnt) hs (float-array cnt)
        ar (float-array cnt) ag (float-array cnt) ab (float-array cnt)
        band (int-array cnt)                 ; palette/freq-band index, for :audio-white? mode
        pal (vec palette) pc (count pal)]
    (dotimes [i cnt]
      (aset xs i (float (* (double (rand)) n)))
      (aset ys i (float (* (double (rand)) n)))
      (aset hs i (float (* (double (rand)) PI2)))
      (aset band i (int (mod i pc)))
      (let [c (nth pal (mod i pc))]
        (aset ar i (float (nth c 0)))
        (aset ag i (float (nth c 1)))
        (aset ab i (float (nth c 2)))))
    {:n n :count cnt :xs xs :ys ys :hs hs :ar ar :ag ag :ab ab :band band
     :trail (float-array (* n n)) :ttmp (float-array (* n n))}))

(defn recolor!
  "Re-paint every agent from `palette` by its band index (mutates the colour
   arrays in place). Band indices are left alone so the audio gains keep driving
   the same groups — only the colour each group wears changes."
  [phys palette]
  (let [cnt (long (:count phys))
        ^floats ar (:ar phys) ^floats ag (:ag phys) ^floats ab (:ab phys)
        ^ints band (:band phys)
        pal (vec palette) pc (count pal)]
    (when (pos? pc)
      (dotimes [i cnt]
        (let [c (nth pal (mod (aget band i) pc))]
          (aset ar i (float (nth c 0)))
          (aset ag i (float (nth c 1)))
          (aset ab i (float (nth c 2))))))
    phys))

(defn- wrap ^double [^double x ^long n]
  (let [m (rem x n)] (if (neg? m) (+ m n) m)))

(defn- sense ^double [^floats fld ^long n ^double x ^double y]
  (aget fld (f/idx n (long (wrap x n)) (long (wrap y n)))))

(defn- diffuse-decay!
  "One separable 3x3 box-blur pass of the trail, then multiply by `keepf`."
  [^long n ^floats trail ^floats tmp ^double keepf]
  (let [third (/ 1.0 3.0)]
    (dotimes [j n]
      (dotimes [i n]
        (let [il (if (zero? i) (dec n) (dec i)) ir (if (= i (dec n)) 0 (inc i))]
          (aset tmp (f/idx n i j)
                (float (* third (+ (aget trail (f/idx n il j)) (aget trail (f/idx n i j))
                                   (aget trail (f/idx n ir j)))))))))
    (dotimes [j n]
      (dotimes [i n]
        (let [jd (if (zero? j) (dec n) (dec j)) ju (if (= j (dec n)) 0 (inc j))]
          (aset trail (f/idx n i j)
                (float (* keepf third (+ (aget tmp (f/idx n i jd)) (aget tmp (f/idx n i j))
                                         (aget tmp (f/idx n i ju)))))))))))

(defn step!
  "Advance agents one tick. Mode (:p-mode p) selects what they sense + deposit:
   :trail => classic white network; :flow => INVISIBLE network that injects its
   motion into the fluid velocity (smoke flows along the traces, agents undrawn);
   otherwise => colour into the smoke."
  [phys fl p]
  (let [n (long (:n phys)) cnt (long (:count phys))
        ^floats xs (:xs phys) ^floats ys (:ys phys) ^floats hs (:hs phys)
        ^floats ar (:ar phys) ^floats ag (:ag phys) ^floats ab (:ab phys)
        ^ints band (:band phys)
        ^floats u (:u fl) ^floats v (:v fl)
        ^floats dr (:dr fl) ^floats dg (:dg fl) ^floats db (:db fl)
        ^floats trail (:trail phys) ^floats ttmp (:ttmp phys)
        trail? (= (:p-mode p) :trail)
        ;; :flow => SLIME agents (sense + deposit their own COLOUR, like :smoke, so
        ;; they form coloured networks and don't blob) that ALSO stir the fluid
        ;; along their heading, so the smoke flows down their coloured traces.
        flow?  (= (:p-mode p) :flow)
        flowk  (double (:p-flow p 6.0))            ; TARGET flow speed agents drive the fluid to (:flow)
        flowa  (double (:p-flow-blend p 0.4))      ; blend rate toward that target (BOUNDS u/v => stable)
        paintw (double (:p-flow-paint p 0.0))      ; :flow agents ALSO paint extra WHITE this much (0 => off)
        ;; :audio-white? => deposit WHITE, fading in each agent's palette colour
        ;; by its freq band's gain (silent band => white, loud band => full colour)
        ^doubles gains (:audio-gains p)
        white?  (boolean (and gains (:audio-white? p)))
        agents? (boolean (and gains (:audio-agents? p)))  ; keep colour, modulate group intensity
        aamp    (double (:audio-agent-amp p 1.5))
        ;; :haze => agents don't steer toward their trail (no networks); they
        ;; random-wander and the fluid carries the colour they deposit => smoke.
        haze?  (= (:p-mode p) :haze)
        wander (double (:p-wander p 0.0))
        so    (double (:p-sensor p)) sa (double (:p-sense-angle p))
        ra    (double (:p-turn p))   ss (double (:p-speed p))
        dep   (double (:p-deposit p)) windk (double (:p-wind p))
        wdep  (* dep (double (:audio-white-density p 0.5)))  ; dimmer deposit for white mode
        ;; FREQ-REACT: split agents into one BUCKET per frequency band; each band's
        ;; gain boosts that bucket's speed (:p-freq-speed) and/or deposit (:p-freq-deposit)
        freq? (boolean (and gains (:p-freq-react? p)))
        nb    (long (if gains (alength gains) 1))
        fsa   (double (:p-freq-speed p 0.0))
        fda   (double (:p-freq-deposit p 0.0))
        ;; ANTAGONISTIC colours: different colour species fight for space.
        ;; :p-antagonist-sense => an agent is attracted to its OWN colour and
        ;;   REPELLED by rival colours (it steers away from foreign trails).
        ;; :p-antagonist       => its deposit ERODES the rival colour channels at
        ;;   the cell, so colours eat into each other => sharp territorial fronts.
        ;; Both generalise over any palette via each agent's colour weights: the
        ;; "rival" weight on channel c is (max_w - w_c), zero for its own dominant
        ;; channel. (Both 0 by default => ordinary independent networks.)
        antag  (double (:p-antagonist p 0.0))
        asens  (double (:p-antagonist-sense p 0.0))
        antag? (and (pos? antag) (not trail?))
        asens? (and (pos? asens) (not trail?))]
    (dotimes [i cnt]
      (let [x (aget xs i) y (aget ys i) h (aget hs i)
            ;; each agent senses ITS OWN colour (weighted density) so the colours
            ;; form separate networks instead of mixing to white. (:trail mode
            ;; senses the shared white trail.)
            wr (aget ar i) wg (aget ag i) wb (aget ab i)
            ;; max colour weight => the agent's own dominant channel. The rival
            ;; weight on channel c is (mw - w_c): 0 for its own, positive for foreign.
            mw (Math/max (double wr) (Math/max (double wg) (double wb)))
            ;; EFFECTIVE sensing weights: own boosted, rivals turned NEGATIVE by
            ;; :p-antagonist-sense so the agent steers away from foreign colours.
            ;; (asens 0 => ewc = wc, the ordinary own-colour sensing.)
            ewr (- (* wr (+ 1.0 asens)) (* asens mw))
            ewg (- (* wg (+ 1.0 asens)) (* asens mw))
            ewb (- (* wb (+ 1.0 asens)) (* asens mw))
            kl (f/idx n (long (wrap (+ x (* so (Math/cos (- h sa)))) n)) (long (wrap (+ y (* so (Math/sin (- h sa)))) n)))
            kc (f/idx n (long (wrap (+ x (* so (Math/cos h))) n))        (long (wrap (+ y (* so (Math/sin h))) n)))
            kr (f/idx n (long (wrap (+ x (* so (Math/cos (+ h sa)))) n)) (long (wrap (+ y (* so (Math/sin (+ h sa)))) n)))
            cl (double (if trail? (aget trail kl) (+ (* ewr (aget dr kl)) (* ewg (aget dg kl)) (* ewb (aget db kl)))))
            cc (double (if trail? (aget trail kc) (+ (* ewr (aget dr kc)) (* ewg (aget dg kc)) (* ewb (aget db kc)))))
            cr (double (if trail? (aget trail kr) (+ (* ewr (aget dr kr)) (* ewg (aget dg kr)) (* ewb (aget db kr)))))
            ;; :haze agents don't steer toward the trail; everyone gets optional
            ;; random wander on top (slime keeps p-wander 0 => unchanged).
            hsteer (if haze? h
                       (cond
                         (and (>= cc cl) (>= cc cr)) h
                         (and (< cc cl) (< cc cr))   (if (< (double (rand)) 0.5) (- h ra) (+ h ra))
                         (> cl cr)                   (- h ra)
                         :else                       (+ h ra)))
            h2 (if (pos? wander) (+ (double hsteer) (* wander 2.0 (- (double (rand)) 0.5))) (double hsteer))
            ;; this agent's frequency-band gain (bucket = even split of agents over bands)
            gband (if freq? (aget gains (min (dec nb) (quot (* i nb) cnt))) 0.0)
            ssi (if freq? (* ss (+ 1.0 (* fsa gband))) ss)   ; band energy => bucket speed
            depi (if freq? (* dep (+ 1.0 (* fda gband))) dep) ; band energy => bucket deposit
            ix (long (wrap x n)) iy (long (wrap y n))
            wk (f/idx n ix iy)
            nx (wrap (+ x (* ssi (Math/cos h2)) (* windk (aget u wk))) n)
            ny (wrap (+ y (* ssi (Math/sin h2)) (* windk (aget v wk))) n)
            dk (f/idx n (long nx) (long ny))]
        (aset xs i (float nx))
        (aset ys i (float ny))
        (aset hs i (float h2))
        (cond
          flow?
          ;; SLIME: deposit the agent's own COLOUR (=> coloured networks, no blob)
          ;; AND push the fluid along its heading so the smoke flows down the trace.
          (do (aset dr dk (+ (aget dr dk) (float (* depi (aget ar i)))))
              (aset dg dk (+ (aget dg dk) (float (* depi (aget ag i)))))
              (aset db dk (+ (aget db dk) (float (* depi (aget ab i)))))
              ;; BLEND u/v toward the agent's heading-flow (bounded => no blow-up):
              ;; u += a·(target − u). |u| stays ≈ flowk regardless of agent count.
              (aset u dk (float (+ (aget u dk) (* flowa (- (* flowk (Math/cos h2)) (aget u dk))))))
              (aset v dk (float (+ (aget v dk) (* flowa (- (* flowk (Math/sin h2)) (aget v dk))))))
              (when (pos? paintw)                         ; optional extra white on top
                (aset dr dk (+ (aget dr dk) (float paintw)))
                (aset dg dk (+ (aget dg dk) (float paintw)))
                (aset db dk (+ (aget db dk) (float paintw)))))
          trail?
          (aset trail dk (+ (aget trail dk) (float dep)))
          agents?
          (let [bi (aget band i)
                e  (if (< bi (alength gains)) (aget gains bi) 0.0)
                d  (* dep (+ 1.0 (* aamp e)))]   ; full colour always; band blooms its group
            (aset dr dk (+ (aget dr dk) (float (* d (aget ar i)))))
            (aset dg dk (+ (aget dg dk) (float (* d (aget ag i)))))
            (aset db dk (+ (aget db dk) (float (* d (aget ab i))))))
          white?
          (let [bi (aget band i)
                e  (if (< bi (alength gains)) (aget gains bi) 0.0)  ; 0 => white, 1 => full colour
                er (* wdep (- 1.0 (* e (- 1.0 (aget ar i)))))
                eg (* wdep (- 1.0 (* e (- 1.0 (aget ag i)))))
                eb (* wdep (- 1.0 (* e (- 1.0 (aget ab i)))))]
            (aset dr dk (+ (aget dr dk) (float er)))
            (aset dg dk (+ (aget dg dk) (float eg)))
            (aset db dk (+ (aget db dk) (float eb))))
          :else
          (do (aset dr dk (+ (aget dr dk) (float (* depi (aget ar i)))))
              (aset dg dk (+ (aget dg dk) (float (* depi (aget ag i)))))
              (aset db dk (+ (aget db dk) (float (* depi (aget ab i)))))))
        ;; ANTAGONISTIC DEPOSIT: erode the rival colour channels at this cell
        ;; (scaled by the agent's own deposit), clamped at 0 => colours eat into
        ;; each other and carve sharp territorial fronts. Own channel = no erosion.
        (when antag?
          (let [er (* antag depi (- mw wr)) eg (* antag depi (- mw wg)) eb (* antag depi (- mw wb))]
            (aset dr dk (float (Math/max 0.0 (- (aget dr dk) er))))
            (aset dg dk (float (Math/max 0.0 (- (aget dg dk) eg))))
            (aset db dk (float (Math/max 0.0 (- (aget db dk) eb))))))))
    (when trail?
      (diffuse-decay! n trail ttmp (double (:p-decay p))))
    phys))
