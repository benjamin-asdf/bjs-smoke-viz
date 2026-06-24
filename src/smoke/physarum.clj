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
   :trail => classic white network; otherwise => colour into the smoke."
  [phys fl p]
  (let [n (long (:n phys)) cnt (long (:count phys))
        ^floats xs (:xs phys) ^floats ys (:ys phys) ^floats hs (:hs phys)
        ^floats ar (:ar phys) ^floats ag (:ag phys) ^floats ab (:ab phys)
        ^ints band (:band phys)
        ^floats u (:u fl) ^floats v (:v fl)
        ^floats dr (:dr fl) ^floats dg (:dg fl) ^floats db (:db fl)
        ^floats trail (:trail phys) ^floats ttmp (:ttmp phys)
        trail? (= (:p-mode p) :trail)
        ;; :audio-white? => deposit WHITE, fading in each agent's palette colour
        ;; by its freq band's gain (silent band => white, loud band => full colour)
        ^doubles gains (:audio-gains p)
        white? (boolean (and gains (:audio-white? p)))
        ;; :haze => agents don't steer toward their trail (no networks); they
        ;; random-wander and the fluid carries the colour they deposit => smoke.
        haze?  (= (:p-mode p) :haze)
        wander (double (:p-wander p 0.0))
        so    (double (:p-sensor p)) sa (double (:p-sense-angle p))
        ra    (double (:p-turn p))   ss (double (:p-speed p))
        dep   (double (:p-deposit p)) windk (double (:p-wind p))
        wdep  (* dep (double (:audio-white-density p 0.5)))]  ; dimmer deposit for white mode
    (dotimes [i cnt]
      (let [x (aget xs i) y (aget ys i) h (aget hs i)
            ;; each agent senses ITS OWN colour (weighted density) so the colours
            ;; form separate networks instead of mixing to white. (:trail mode
            ;; senses the shared white trail.)
            wr (aget ar i) wg (aget ag i) wb (aget ab i)
            kl (f/idx n (long (wrap (+ x (* so (Math/cos (- h sa)))) n)) (long (wrap (+ y (* so (Math/sin (- h sa)))) n)))
            kc (f/idx n (long (wrap (+ x (* so (Math/cos h))) n))        (long (wrap (+ y (* so (Math/sin h))) n)))
            kr (f/idx n (long (wrap (+ x (* so (Math/cos (+ h sa)))) n)) (long (wrap (+ y (* so (Math/sin (+ h sa)))) n)))
            cl (double (if trail? (aget trail kl) (+ (* wr (aget dr kl)) (* wg (aget dg kl)) (* wb (aget db kl)))))
            cc (double (if trail? (aget trail kc) (+ (* wr (aget dr kc)) (* wg (aget dg kc)) (* wb (aget db kc)))))
            cr (double (if trail? (aget trail kr) (+ (* wr (aget dr kr)) (* wg (aget dg kr)) (* wb (aget db kr)))))
            ;; :haze agents don't steer toward the trail; everyone gets optional
            ;; random wander on top (slime keeps p-wander 0 => unchanged).
            hsteer (if haze? h
                       (cond
                         (and (>= cc cl) (>= cc cr)) h
                         (and (< cc cl) (< cc cr))   (if (< (double (rand)) 0.5) (- h ra) (+ h ra))
                         (> cl cr)                   (- h ra)
                         :else                       (+ h ra)))
            h2 (if (pos? wander) (+ (double hsteer) (* wander 2.0 (- (double (rand)) 0.5))) (double hsteer))
            ix (long (wrap x n)) iy (long (wrap y n))
            wk (f/idx n ix iy)
            nx (wrap (+ x (* ss (Math/cos h2)) (* windk (aget u wk))) n)
            ny (wrap (+ y (* ss (Math/sin h2)) (* windk (aget v wk))) n)
            dk (f/idx n (long nx) (long ny))]
        (aset xs i (float nx))
        (aset ys i (float ny))
        (aset hs i (float h2))
        (cond
          trail?
          (aset trail dk (+ (aget trail dk) (float dep)))
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
          (do (aset dr dk (+ (aget dr dk) (float (* dep (aget ar i)))))
              (aset dg dk (+ (aget dg dk) (float (* dep (aget ag i)))))
              (aset db dk (+ (aget db dk) (float (* dep (aget ab i)))))))))
    (when trail?
      (diffuse-decay! n trail ttmp (double (:p-decay p))))
    phys))
