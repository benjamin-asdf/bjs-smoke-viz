(ns smoke.controls
  "A second window of live knobs for the smoke sketch. Every control writes
   straight into `smoke.core/params` (the atom the render loop reads each frame),
   so dragging a slider steers the running simulation immediately.

   Theme / agent-count changes need the field re-seeded, so they flip
   `smoke.core/reset?` — the sketch rebuilds the fluid + agents next frame.

     (smoke.controls/open!)   ; opened automatically by smoke.core/start!
     (smoke.controls/close!)  ; dispose the panel"
  (:require [smoke.core :as core]
            [smoke.scene :as scene]
            [clojure.string :as str])
  (:import [javax.swing JFrame JPanel JSlider JLabel JComboBox JCheckBox
            JButton JSpinner SpinnerNumberModel JScrollPane BoxLayout
            BorderFactory JColorChooser JTextField]
           [javax.swing.event ChangeListener]
           [java.awt BorderLayout Dimension Font GridLayout FlowLayout Color]
           [java.awt.event ActionListener]))

(defonce frame (atom nil))
(declare open! close! refresh!)

(def ^:private RES 1000)  ; slider integer resolution
(def ^:private FONT (Font. "SansSerif" Font/PLAIN 16))
(def ^:private FONT-BOLD (Font. "SansSerif" Font/BOLD 18))

(defn- enable-text-aa!
  "Turn on Swing text antialiasing. On Linux the desktop AA hint is often unset,
   so labels render aliased. Swing reads the \"awt.font.desktophints\" desktop
   property each paint; we install a map that requests AA. setDesktopProperty is
   protected, hence the reflection."
  []
  (try
    (let [tk    (java.awt.Toolkit/getDefaultToolkit)
          hints (doto (java.util.HashMap.)
                  (.put java.awt.RenderingHints/KEY_TEXT_ANTIALIASING
                        java.awt.RenderingHints/VALUE_TEXT_ANTIALIAS_ON))
          m     (.getDeclaredMethod java.awt.Toolkit "setDesktopProperty"
                                    (into-array Class [String Object]))]
      (.setAccessible m true)
      (.invoke m tk (object-array ["awt.font.desktophints" hints])))
    (catch Throwable _)))

;; [key label lo hi int?] — sliders, in display order. :section starts a group.
(def ^:private specs
  [[:section "Fluid"]
   [:dt          "dt — sim speed"      0.0   0.5]
   [:visc        "viscosity"           0.0   1.0]
   [:buoy        "buoyancy"            0.0   3.0]
   [:keep        "density keep"        0.9   1.0]
   [:edge-margin "edge margin"         0     40    :int]
   [:wind        "wind"                0.0   20.0]
   [:noise-scale "wind scale"          0.2   8.0]
   [:noise-speed "wind speed"          0.0   0.1]
   [:section "Render"]
   [:blur-passes "blur passes"         0     5     :int]
   [:expos       "exposure"            0.2   10.0]
   [:saturation  "saturation (vivid)"  1.0   6.0]
   [:section "Physarum (slime / haze / swarm / rivers / network)"]
   [:p-sensor    "sensor distance"     0.0   30.0]
   [:p-sense-angle "sensor angle"      0.0   1.5]
   [:p-turn      "turn strength"       0.0   1.5]
   [:p-wander    "wander (haze)"       0.0   2.0]
   [:p-speed     "agent speed"         0.0   5.0]
   [:p-deposit   "deposit"             0.0   1.0]
   [:p-wind      "fluid drag"          0.0   2.0]
   [:p-decay     "trail decay"         0.5   1.0]
   [:p-bright    "network brightness"  0.0   2.0]
   [:section "Stars"]
   [:star-thresh "threshold"           0.0   20.0]
   [:star-radius "radius"              1     10    :int]
   [:star-speed  "twinkle speed"       0.0   2.0]])

(defn- fmt [int? v]
  (if int? (str (long v)) (format "%.4g" (double v))))

(defn- slider-row [^JPanel parent k label lo hi int?]
  (let [lo (double lo) hi (double hi)
        ->tick (fn [v] (int (Math/round (* RES (/ (- (double v) lo) (- hi lo))))))
        ->val  (fn [t] (let [v (+ lo (* (/ (double t) RES) (- hi lo)))]
                         (if int? (double (Math/round v)) v)))
        cur    (double (get @core/params k lo))
        slider (JSlider. 0 RES (max 0 (min RES (->tick cur))))
        lbl    (JLabel. ^String (str label "  =  " (fmt int? cur)))
        row    (JPanel. (BorderLayout. 6 0))]
    (.addChangeListener slider
                        (reify ChangeListener
                          (stateChanged [_ _]
                            (let [v (->val (.getValue slider))]
                              (swap! core/params assoc k v)
                              (.setText lbl (str label "  =  " (fmt int? v)))))))
    (.setFont lbl FONT)
    (doto row
      (.setAlignmentX 0.0)
      (.add lbl BorderLayout/NORTH)
      (.add slider BorderLayout/CENTER))
    (.add parent row)))

(defn- section [^JPanel parent ^String title]
  (let [l (JLabel. title)]
    (.setFont l FONT-BOLD)
    (.setBorder l (BorderFactory/createEmptyBorder 12 0 2 0))
    (.setAlignmentX l (float 0.0))
    (.add parent l)))

(defn- theme-row [^JPanel parent]
  (let [themes (vec (keys scene/themes))
        combo  (JComboBox. (into-array String (map name themes)))
        row    (JPanel. (BorderLayout. 6 0))]
    (.setSelectedItem combo (name (:theme @core/params)))
    (.addActionListener combo
                        (reify ActionListener
                          (actionPerformed [_ _]
                            (let [t (nth themes (.getSelectedIndex combo))]
                              ;; apply the theme's own parameter character, then re-seed
                              (swap! core/params #(merge (assoc % :theme t) (scene/theme-defaults t)))
                              (reset! core/reset? true)
                              (refresh!)))))   ; sliders re-read the theme's defaults
    (.setFont combo FONT)
    (doto row
      (.setAlignmentX 0.0)
      (.add (doto (JLabel. "theme") (.setFont FONT)) BorderLayout/WEST)
      (.add combo BorderLayout/CENTER))
    (.add parent row)))

(defn- ->awt ^Color [[r g b]]
  (Color. (float (min 1.0 (double r))) (float (min 1.0 (double g))) (float (min 1.0 (double b)))))
(defn- ->rgb [^Color c]
  [(/ (.getRed c) 255.0) (/ (.getGreen c) 255.0) (/ (.getBlue c) 255.0)])
(defn- ->hex [[r g b]]
  (format "#%02X%02X%02X"
          (int (Math/round (* 255.0 (min 1.0 (double r)))))
          (int (Math/round (* 255.0 (min 1.0 (double g)))))
          (int (Math/round (* 255.0 (min 1.0 (double b)))))))
(defn- hex->rgb [s]
  (let [h (str/replace (str/trim (or s "")) #"^#" "")]
    (when (re-matches #"(?i)[0-9a-f]{6}" h)
      [(/ (Integer/parseInt (subs h 0 2) 16) 255.0)
       (/ (Integer/parseInt (subs h 2 4) 16) 255.0)
       (/ (Integer/parseInt (subs h 4 6) 16) 255.0)])))

(defn- jet-row
  "Colour controls for the single :jet1 source: summerfest preset dropdown, a
   hex field (paste #rrggbb + Enter), and a free colour picker. All write
   (:jet-color params); the swatch + hex field show the current colour."
  [^JPanel parent]
  (let [palettes scene/jet-palettes
        combo    (JComboBox. (into-array String (mapv (comp name first) palettes)))
        swatch   (JButton. "Pick…")
        hex      (JTextField. 8)
        cur      (or (:jet-color @core/params) [1.0 1.0 1.0])
        show!    (fn [col] (.setBackground swatch (->awt col)) (.setText hex (->hex col)))
        set-col! (fn [col] (swap! core/params assoc :jet-color col) (show! col))
        r1       (JPanel. (BorderLayout. 6 0))
        r2       (JPanel. (FlowLayout. FlowLayout/LEFT 6 0))]
    (doseq [^java.awt.Component c [combo swatch hex]] (.setFont c FONT))
    (.setOpaque swatch true)
    (show! cur)
    (.setSelectedIndex combo -1)   ; no preset chosen yet => construction fires nothing
    (.addActionListener combo
                        (reify ActionListener
                          (actionPerformed [_ _]
                            (let [i (.getSelectedIndex combo)]
                              (when (>= i 0) (set-col! (second (nth palettes i))))))))
    (.addActionListener swatch
                        (reify ActionListener
                          (actionPerformed [_ _]
                            (when-let [c (JColorChooser/showDialog swatch "Jet colour" (->awt (or (:jet-color @core/params) cur)))]
                              (set-col! (->rgb c))))))
    (.addActionListener hex      ; Enter in the field applies the pasted hex
                        (reify ActionListener
                          (actionPerformed [_ _]
                            (if-let [col (hex->rgb (.getText hex))]
                              (set-col! col)
                              (show! (or (:jet-color @core/params) cur))))))  ; bad input => revert
    (doto r1
      (.setAlignmentX 0.0)
      (.add (doto (JLabel. "jet palette") (.setFont FONT)) BorderLayout/WEST)
      (.add combo BorderLayout/CENTER))
    (doto r2
      (.setAlignmentX 0.0)
      (.add (doto (JLabel. "hex") (.setFont FONT)))
      (.add hex)
      (.add swatch))
    (.add parent r1)
    (.add parent r2)))

(defn- count-row [^JPanel parent]
  (let [model  (SpinnerNumberModel. (int (:p-count @core/params)) (int 0) (int 60000) (int 500))
        spin   (JSpinner. model)
        row    (JPanel. (BorderLayout. 6 0))]
    (.addChangeListener spin
                        (reify ChangeListener
                          (stateChanged [_ _] (swap! core/params assoc :p-count (.getValue spin)))))
    (.setFont spin FONT)
    (doto row
      (.setAlignmentX 0.0)
      (.add (doto (JLabel. "agent count (applies on reset)") (.setFont FONT)) BorderLayout/WEST)
      (.add spin BorderLayout/EAST))
    (.add parent row)))

(defn- buttons-row [^JPanel parent]
  (let [stars (JCheckBox. "stars" (boolean (:stars @core/params)))
        reset (JButton. "Reset field (r)")
        pause (JButton. "Toggle pause (space)")
        defs  (JButton. "Reset all params")
        rest- (JButton. "Restart window")
        row   (JPanel. (GridLayout. 0 2 6 4))]
    (.addActionListener stars
                        (reify ActionListener
                          (actionPerformed [_ _] (swap! core/params assoc :stars (.isSelected stars)))))
    (.addActionListener reset
                        (reify ActionListener
                          (actionPerformed [_ _] (reset! core/reset? true))))
    (.addActionListener pause
                        (reify ActionListener
                          (actionPerformed [_ _] (reset! core/pause-flip? true))))
    (.addActionListener defs
                        (reify ActionListener
                          (actionPerformed [_ _]
                            (reset! core/params scene/default-params)  ; all knobs back to defaults
                            (reset! core/reset? true)                  ; re-seed (theme/agents may change)
                            (refresh!))))                              ; rebuild in place (no window flash)
    (.addActionListener rest-
                        (reify ActionListener
                          (actionPerformed [_ _]
                            ;; off the EDT: q/sketch spawns its own window/threads
                            (.start (Thread. ^Runnable (fn [] (smoke.core/restart!)))))))
    (doseq [^java.awt.Component c [stars reset pause defs rest-]] (.setFont c FONT))
    (doto row (.setAlignmentX 0.0) (.add stars) (.add reset) (.add pause) (.add defs) (.add rest-))
    (.add parent row)))

(defn- populate! [^JPanel panel]
  (section panel "Scene")
  (theme-row panel)
  (jet-row panel)
  (buttons-row panel)
  (doseq [spec specs]
    (if (= (first spec) :section)
      (section panel (second spec))
      (let [[k label lo hi int?] spec]
        (slider-row panel k label lo hi (= int? :int)))))
  (section panel "Physarum agents")
  (count-row panel)
  panel)

(defn refresh!
  "Rebuild the panel contents in place so sliders re-read params — no window
   flash (used by Reset all params). Runs on the EDT."
  []
  (when-let [^JFrame f @frame]
    (let [^JScrollPane sp (.getContentPane f)
          ^JPanel panel (.. sp getViewport getView)]
      (.removeAll panel)
      (populate! panel)
      (.revalidate panel)
      (.repaint panel))))

(defn open!
  "Open (or focus) the controls window."
  []
  (enable-text-aa!)
  (if-let [^JFrame f @frame]
    (doto f (.setVisible true) (.toFront))
    (let [panel (JPanel.)
          fr    (JFrame. "bjs-smoke-viz — controls")]
      (.setLayout panel (BoxLayout. panel BoxLayout/Y_AXIS))
      (.setBorder panel (BorderFactory/createEmptyBorder 8 10 8 10))
      (populate! panel)
      (doto fr
        (.setContentPane (JScrollPane. panel))
        (.setSize (Dimension. 460 860))
        (.setDefaultCloseOperation JFrame/HIDE_ON_CLOSE)
        (.setVisible true))
      (reset! frame fr)
      fr)))

(defn close! []
  (when-let [^JFrame f @frame] (.dispose f) (reset! frame nil)))
