(ns hoatzin.app.input.fields
  "Input to boxes (see hoatzin.lib.ui), and to the window open over the
  text.

  Clicking an editable field gives it the focus: then typing goes into it,
  backspace takes from its end, up and down step an integer field, tab and
  shift-tab move to the next and previous field, and return or a click
  elsewhere gives the focus up. A click on a checkbox toggles it."
  (:require [hoatzin.app.geometry :refer [view-height]]
            [hoatzin.app.settings-window :as settings-window]
            [hoatzin.app.state :refer [px]]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.ui :as ui]))

(defn ui-value
  "What interactive box `node` holds: what it was set to, else its :value."
  [app {:keys [id value]}]
  (get (:ui-values app) id value))

(defn ui-hit
  "The box under render pixel (x, y), as hoatzin.lib.ui/hit gives it:
  floats before blocks, and blocks only where the text is in view."
  [app x y]
  (or (ui/hit (:float-places app) x y)
      (let [m (px app (:margin app))]
        (when (and (>= y m) (< y (+ m (view-height app))))
          (some #(ui/hit (:placed %) (- x m) (+ (- y m) (:scroll app)))
                (:block-places app))))))

;; ---------------------------------------------------------------- the focus

(defn- field-places
  "Every editable field placed, in order (the floats', then the blocks'),
  as hoatzin.lib.ui/place gives them but in render pixels."
  [app]
  (let [m (px app (:margin app))]
    (filterv #(ui/editable? (:node %))
             (concat (:float-places app)
                     (mapcat #(ui/offset (:placed %) m (- m (:scroll app))) (:block-places app))))))

(defn focused-field
  "The field with the focus, as `field-places` has it, or nil."
  [app]
  (when-let [id (:focus app)]
    (some #(when (= id (get-in % [:node :id])) %) (field-places app))))

(defn blur
  "Give up the focus. A settings field shows its setting again, rather
  than what was typed."
  [app]
  (if-let [id (:focus app)]
    (cond-> (-> app (dissoc :focus) (assoc :dirty? true))
      (settings-window/fields id) (update :ui-values dissoc id))
    app))

(defn- focus [app now id]
  (-> (if (= id (:focus app)) app (blur app))
      (assoc :focus id :dirty? true :blink-from now)))

(defn- next-field
  "Move the focus `delta` fields on, wrapping around."
  [app now delta]
  (let [ids (mapv #(get-in % [:node :id]) (field-places app))
        i   (or (first (keep-indexed #(when (= %2 (:focus app)) %1) ids)) 0)]
    (focus app now (ids (mod (+ i delta) (count ids))))))

;; ---------------------------------------------------------------- editing

(defn- set-field
  "Field `node` holds `value` now. A settings field changes its setting."
  [app now node value]
  (let [id  (:id node)
        app (-> app (assoc-in [:ui-values id] value) (assoc :dirty? true :blink-from now))]
    (if-let [path (settings-window/fields id)]
      (settings-window/change-setting app now path (parse-long value))
      app)))

(defn on-ui-click
  "A click on a box: it doesn't reach the text. An editable field takes
  the focus; a checkbox toggles."
  [app now {:keys [node]}]
  (cond
    (ui/editable? node)        (focus app now (:id node))
    (= :checkbox (:kind node)) (-> (blur app)
                                   (assoc-in [:ui-values (:id node)] (not (ui-value app node)))
                                   (assoc :dirty? true))
    :else                      (blur app)))

(defn- close-window [app]
  (-> (blur app) (assoc :window nil :dirty? true)))

(defn- on-field-key
  "A key while field `node` has the focus. Other keys do nothing."
  [app now node key mod]
  (let [value (str (ui-value app node))
        step  #(if-let [v (ui/stepped node value %)] (set-field app now node v) app)]
    (condp = key
      sdl/K-ESCAPE    (if (:window app) (close-window app) (blur app))
      sdl/K-RETURN    (blur app)
      sdl/K-KP-ENTER  (blur app)
      sdl/K-TAB       (next-field app now (if (pos? (bit-and mod sdl/KMOD-SHIFT)) -1 1))
      sdl/K-BACKSPACE (if (seq value) (set-field app now node (subs value 0 (dec (count value)))) app)
      sdl/K-UP        (step 1)
      sdl/K-DOWN      (step -1)
      app)))

;; ---------------------------------------------------------------- events

(defn on-focus-event
  "An event while a field has the focus: keys and typing go to it. nil for
  the events it leaves to the rest of the app."
  [app now event]
  (let [{:keys [node]} (focused-field app)]
    (case (:type event)
      :key  (on-field-key app now node (:key event) (:mod event 0))
      :text (set-field app now node (ui/typed node (ui-value app node) (:text event)))
      :composition app
      nil)))

(defn on-window-event
  "An event while a window is open: escape closes it, and clicks go to its
  boxes. The text takes nothing. nil for the events it leaves to the rest
  of the app."
  [app now event]
  (case (:type event)
    :key   (if (= sdl/K-ESCAPE (:key event)) (close-window app) app)
    :click (if-let [hit (ui-hit app (:x event) (:y event))] (on-ui-click app now hit) (blur app))
    (:text :composition :drag :wheel) app
    nil))
