(ns hoatzin.app.theme
  "Themes: a name and three colours, [r g b] each:
    :background  the main background
    :text        the text's colour
    :accent      the status bar, and what else stands out

  `colours` works every other colour the app draws with out of them, as the
  app's keys (see hoatzin.app.state/defaults): those that stand between
  background and text are mixes of the two, and those that mark something
  are mixes of the background and the accent. The status bar is the
  accent, in whichever of the text and the background reads better on it;
  the windows over the text are a little off the background.")

(defn- rgb [n]
  [(bit-and (bit-shift-right n 16) 0xFF) (bit-and (bit-shift-right n 8) 0xFF) (bit-and n 0xFF)])

(defn- theme [name background text accent]
  {:name name :background (rgb background) :text (rgb text) :accent (rgb accent)})

(def themes
  "The themes there are, in the order they are listed."
  [(theme "Light 1" 0xFFFFFF 0x000000 0x5A7D7C)
   (theme "Light 2" 0xFFFFFF 0x000000 0xA3320B)
   (theme "Light 3" 0xFFFFFF 0x232C33 0xF786AA)
   (theme "Dark 1"  0x000000 0xFFFFFF 0x5A7D7C)
   (theme "Dark 2"  0x000000 0xFFFFFF 0xA3320B)
   (theme "Dark 3"  0x232C33 0xFFFFFF 0xF786AA)])

(def default-name "The theme the settings begin with." "Dark 3")

(def names (mapv :name themes))

(defn find-theme
  "The theme called `name`, or nil."
  [name]
  (first (filter #(= name (:name %)) themes)))

(defn swatches
  "Each theme's colours, [background text accent], by name."
  []
  (into {} (map (juxt :name (juxt :background :text :accent))) themes))

(defn- mix
  "Colour `a` `t` of the way to colour `b`, 0 to 1."
  [a b t]
  (mapv #(long (Math/round (+ (* (- 1.0 t) %1) (* t %2)))) a b))

(defn- channel [c]
  (let [c (/ c 255.0)]
    (if (<= c 0.03928) (/ c 12.92) (Math/pow (/ (+ c 0.055) 1.055) 2.4))))

(defn- luminance [[r g b]]
  (+ (* 0.2126 (channel r)) (* 0.7152 (channel g)) (* 0.0722 (channel b))))

(defn- contrast [a b]
  (let [[hi lo] (sort > [(luminance a) (luminance b)])]
    (/ (+ hi 0.05) (+ lo 0.05))))

(defn- on
  "Whichever of `a` and `b` reads better on `ground`."
  [ground a b]
  (if (>= (contrast ground a) (contrast ground b)) a b))

(defn colours
  "The app's colour keys for the theme called `name` (the default's, if
  there is none such)."
  [name]
  (let [{:keys [background text accent]} (or (find-theme name) (find-theme default-name))
        bg->text #(mix background text %)
        bg->acc  #(mix background accent %)]
    {:background background
     :foreground text
     :selection (bg->acc 0.45)
     :selection-unfocused (bg->acc 0.2)
     :scrollbar-track (bg->text 0.08)
     :scrollbar-thumb (bg->text 0.3)
     :scrollbar-thumb-active (bg->text 0.55)
     :status-background accent
     :status-foreground (on accent text background)
     :window-background (bg->text 0.07)
     :ui-dim (bg->text 0.65)
     :ui-border (bg->text 0.4)
     :ui-accent accent
     :ui-focus text
     :ui-field-background background
     :ui-hover (bg->text 0.1)
     :ui-highlight (bg->acc 0.45)}))
