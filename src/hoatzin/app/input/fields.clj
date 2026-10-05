(ns hoatzin.app.input.fields
  "Input to boxes (see hoatzin.lib.ui), and to the window open over the
  text.

  Clicking an editable field gives it the focus: then typing goes into it,
  backspace takes from its end, up and down step an integer field, tab and
  shift-tab move to the next and previous field or dropdown, and return or
  a click elsewhere gives the focus up. A click on a checkbox toggles it.

  Clicking a dropdown gives it the focus and opens its list (see
  hoatzin.app.dropdown). With the focus, return, up, down or typing open
  it too; typing goes on into the list, to the option it begins."
  (:require [hoatzin.app.boxes :refer [focusable-places focused-field ui-hit ui-value]]
            [hoatzin.app.dropdown :as dropdown]
            [hoatzin.app.help-window :as help-window]
            [hoatzin.app.settings-window :as settings-window]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.ui :as ui]))

;; ---------------------------------------------------------------- the focus

(defn blur
  "Give up the focus, closing any list. A settings field shows its
  setting again, rather than what was typed."
  [app]
  (if-let [id (:focus app)]
    (cond-> (-> app dropdown/close (dissoc :focus) (assoc :dirty? true))
      (settings-window/fields id) (update :ui-values dissoc id))
    app))

(defn- focus [app now id]
  (-> (if (= id (:focus app)) (dropdown/close app) (blur app))
      (assoc :focus id :dirty? true :blink-from now)))

(defn- next-field
  "Move the focus `delta` fields on, wrapping around."
  [app now delta]
  (let [ids (mapv #(get-in % [:node :id]) (focusable-places app))
        i   (or (first (keep-indexed #(when (= %2 (:focus app)) %1) ids)) 0)]
    (focus app now (ids (mod (+ i delta) (count ids))))))

;; ---------------------------------------------------------------- editing

(defn- set-field
  "Field `node` holds `value` now. A settings field changes its setting."
  [app now node value]
  (-> (settings-window/set-value app now (:id node) value)
      (assoc :blink-from now)))

(defn on-ui-click
  "A click on a box: it doesn't reach the text. An editable field takes
  the focus, a dropdown takes it and opens its list, and a checkbox
  toggles."
  [app now {:keys [node]}]
  (cond
    (ui/editable? node)        (focus app now (:id node))
    (= :dropdown (:kind node)) (-> (focus app now (:id node)) (dropdown/open now))
    (= :checkbox (:kind node)) (-> (blur app)
                                   (assoc-in [:ui-values (:id node)]
                                             (not (ui-value app node)))
                                   (assoc :dirty? true))
    :else                      (blur app)))

(defn- close-window [app]
  (-> (blur app) (assoc :window nil :dirty? true)))

(defn- on-field-key
  "A key while field or dropdown `node` has the focus. Other keys do
  nothing."
  [app now node key mod]
  (let [value     (str (ui-value app node))
        dropdown? (= :dropdown (:kind node))
        step      #(if-let [v (ui/stepped node value %)] (set-field app now node v) app)]
    (condp = key
      sdl/K-ESCAPE    (if (:window app) (close-window app) (blur app))
      sdl/K-TAB       (next-field app now (if (pos? (bit-and mod sdl/KMOD-SHIFT)) -1 1))
      sdl/K-RETURN    (if dropdown? (dropdown/open app now) (blur app))
      sdl/K-KP-ENTER  (if dropdown? (dropdown/open app now) (blur app))
      sdl/K-UP        (if dropdown? (dropdown/open app now) (step 1))
      sdl/K-DOWN      (if dropdown? (dropdown/open app now) (step -1))
      sdl/K-BACKSPACE (if (and (not dropdown?) (seq value))
                        (set-field app now node (subs value 0 (dec (count value))))
                        app)
      app)))

(defn- on-field-text
  "Typed `text` while field or dropdown `node` has the focus. A dropdown
  opens its list, and a letter goes on into it."
  [app now node text]
  (if (= :dropdown (:kind node))
    (let [app (dropdown/open app now)]
      (if (and (:list app) (not= " " text)) (dropdown/on-text app now text) app))
    (set-field app now node (ui/typed node (ui-value app node) text))))

;; ---------------------------------------------------------------- events

(defn on-focus-event
  "An event while a field or dropdown has the focus: keys and typing go
  to it. nil for the events it leaves to the rest of the app."
  [app now event]
  (let [{:keys [node]} (focused-field app)]
    (case (:type event)
      :key  (on-field-key app now node (:key event) (:mod event 0))
      :text (on-field-text app now node (:text event))
      :composition app
      nil)))

(defn on-window-event
  "An event while a window is open: escape closes it, and clicks go to its
  boxes. The text takes nothing. nil for the events it leaves to the rest
  of the app."
  [app now event]
  (case (:type event)
    :key   (cond (= sdl/K-ESCAPE (:key event)) (close-window app)
                 (= :help (:window app))       (help-window/on-key app (:key event))
                 :else                         app)
    :click (if-let [hit (ui-hit app (:x event) (:y event))] (on-ui-click app now hit) (blur app))
    :wheel (if (= :help (:window app)) (help-window/on-wheel app (:dy event)) app)
    (:text :composition :drag) app
    nil))
