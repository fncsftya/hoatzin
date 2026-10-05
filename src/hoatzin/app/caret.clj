(ns hoatzin.app.caret
  "The caret: where it is drawn, its shape, and when it blinks. It is in
  the field with the focus if there is one, else on the command line if
  that is open, else in the text: a bar in insert mode and a block in
  normal mode."
  (:require [hoatzin.app.command :as command]
            [hoatzin.app.display :refer [view-caret]]
            [hoatzin.app.face :refer [face-line ui-width]]
            [hoatzin.app.geometry :as geo]
            [hoatzin.app.input.fields :refer [focused-field ui-value]]
            [hoatzin.app.state :refer [px insert? command?]]
            [hoatzin.lib.coretext :as ct]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.layout :as layout]))

;; ---------------------------------------------------------------- where

(defn- block-extent
  "The block caret's [x0 x1]: over the character after the caret, or half
  an em wide where there is none on its line (at a line's end)."
  [app x k]
  (let [L    (:layout app)
        pos  (view-caret app)
        next (layout/next-position L pos)
        [x1 k1] (layout/caret L next true)]
    (if (and (< pos next) (= k k1) (not= x x1))
      [(min x x1) (max x x1)]
      [x (+ x (px app (/ (get-in app [:settings :editor-font :size]) 2)))])))

(defn- text-caret-rect
  "A bar in insert mode, a block in normal mode."
  [app]
  (let [{:keys [layout scroll]} app
        {:keys [caret-top caret-height]} (:metrics layout)
        m (px app (:margin app))
        [x k] (geo/caret-place app)
        y (+ m (- (geo/line-top app k) scroll) caret-top)]
    (if (insert? app)
      [(+ m (long (Math/floor x))) y (max 1 (px app 1)) caret-height]
      (let [[x0 x1] (block-extent app x k)
            x0 (long (Math/floor x0))]
        [(+ m x0) y (max 1 (- (long (Math/ceil x1)) x0)) caret-height]))))

(defn- command-caret-rect
  "A bar at the end of the command line."
  [{:keys [ui] :as app}]
  (let [{:keys [caret-top caret-height]} (:metrics ui)
        {:keys [length] :as ln} (face-line ui (command/line-text app))
        x (ct/offset-for-index ln length)]
    [(+ (px app (:margin app)) (long (Math/floor x)))
     (+ (geo/text-height app) (px app (:status-padding app)) caret-top)
     (max 1 (px app 1))
     caret-height]))

(defn- field-caret-rect
  "A bar at the end of the text in the field with the focus, or nil."
  [app]
  (when-let [{:keys [node] [cx cy _ ch] :content} (focused-field app)]
    (let [{:keys [line-height caret-top caret-height]} (get-in app [:ui :metrics])]
      [(+ cx (ui-width app (str (ui-value app node))))
       (+ cy (quot (- ch line-height) 2) caret-top)
       (max 1 (px app 1))
       caret-height])))

(defn caret-rect
  "The caret's [x y w h] in render pixels. While a field has the focus, or
  there is a command line, the caret is there, not in the text."
  [app]
  (or (when (:focus app) (field-caret-rect app))
      (if (command? app) (command-caret-rect app) (text-caret-rect app))))

;; ---------------------------------------------------------------- blinking

(defn- caret-in-view? [app]
  (let [[_ y _ h] (caret-rect app)
        m (px app (:margin app))]
    (and (< y (+ m (geo/view-height app))) (> (+ y h) m))))

(defn caret-blinking?
  "The caret shows while focused. In the text, it shows while no window
  is open and nothing is selected (a selection replaces it), and in view.
  In a field with the focus, or on the command line, it always shows."
  [app]
  (and (:focused? app)
       (cond (:focus app)   true
             (:window app)  false
             (command? app) true
             :else (and (nil? (ed/selection (:doc app))) (caret-in-view? app)))))

(defn caret-visible?
  "Whether the caret is shown at time `now` (ms): it is blinking, and in
  the on half of the blink."
  [app now]
  (and (caret-blinking? app) (even? (quot (- now (:blink-from app)) (:blink-ms app)))))
