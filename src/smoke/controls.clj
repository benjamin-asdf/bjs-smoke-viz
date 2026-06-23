(ns smoke.controls
  "A second window of live knobs for the smoke sketch. Every control writes
   straight into `smoke.core/params` (the atom the render loop reads each frame),
   so dragging a slider steers the running simulation immediately.

   Theme / agent-count changes need the field re-seeded, so they flip
   `smoke.core/reset?` — the sketch rebuilds the fluid + agents next frame.

     (smoke.controls/open!)   ; opened automatically by smoke.core/start!
     (smoke.controls/close!)  ; dispose the panel"
  (:require [smoke.core :as core]
            [smoke.scene :as scene])
  (:import [javax.swing JFrame JPanel JSlider JLabel JComboBox JCheckBox
            JButton JSpinner SpinnerNumberModel JScrollPane BoxLayout
            BorderFactory]
           [javax.swing.event ChangeListener]
           [java.awt BorderLayout Dimension Font GridLayout]
           [java.awt.event ActionListener]))

(defonce frame (atom nil))

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
   [:visc        "viscosity"           0.0   0.02]
   [:buoy        "buoyancy"            0.0   3.0]
   [:keep        "density keep"        0.9   1.0]
   [:edge-margin "edge margin"         0     40    :int]
   [:wind        "wind"                0.0   20.0]
   [:noise-scale "wind scale"          0.2   8.0]
   [:noise-speed "wind speed"          0.0   0.1]
   [:section "Render"]
   [:blur-passes "blur passes"         0     5     :int]
   [:expos       "exposure"            0.2   5.0]
   [:section "Physarum (slime / network)"]
   [:p-sensor    "sensor distance"     0.0   30.0]
   [:p-sense-angle "sensor angle"      0.0   1.5]
   [:p-turn      "turn strength"       0.0   1.5]
   [:p-speed     "agent speed"         0.0   5.0]
   [:p-deposit   "deposit"             0.0   1.0]
   [:p-wind      "fluid drag"          0.0   2.0]
   [:p-decay     "trail decay"         0.5   1.0]
   [:p-bright    "network brightness"  0.0   2.0]
   [:section "Stars"]
   [:star-thresh "threshold"           0.0   6.0]
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
                            (swap! core/params assoc :theme (nth themes (.getSelectedIndex combo)))
                            (reset! core/reset? true))))   ; re-seed: palette/agents depend on theme
    (.setFont combo FONT)
    (doto row
      (.setAlignmentX 0.0)
      (.add (doto (JLabel. "theme") (.setFont FONT)) BorderLayout/WEST)
      (.add combo BorderLayout/CENTER))
    (.add parent row)))

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
        row   (JPanel. (GridLayout. 1 3 6 0))]
    (.addActionListener stars
                        (reify ActionListener
                          (actionPerformed [_ _] (swap! core/params assoc :stars (.isSelected stars)))))
    (.addActionListener reset
                        (reify ActionListener
                          (actionPerformed [_ _] (reset! core/reset? true))))
    (.addActionListener pause
                        (reify ActionListener
                          (actionPerformed [_ _] (reset! core/pause-flip? true))))
    (doseq [^java.awt.Component c [stars reset pause]] (.setFont c FONT))
    (doto row (.setAlignmentX 0.0) (.add stars) (.add reset) (.add pause))
    (.add parent row)))

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
      (section panel "Scene")
      (theme-row panel)
      (buttons-row panel)
      (doseq [spec specs]
        (if (= (first spec) :section)
          (section panel (second spec))
          (let [[k label lo hi int?] spec]
            (slider-row panel k label lo hi (= int? :int)))))
      (section panel "Physarum agents")
      (count-row panel)
      (doto fr
        (.setContentPane (JScrollPane. panel))
        (.setSize (Dimension. 460 860))
        (.setDefaultCloseOperation JFrame/HIDE_ON_CLOSE)
        (.setVisible true))
      (reset! frame fr)
      fr)))

(defn close! []
  (when-let [^JFrame f @frame] (.dispose f) (reset! frame nil)))
