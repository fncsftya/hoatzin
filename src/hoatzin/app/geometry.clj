(ns hoatzin.app.geometry
  "Where things are on screen. The window is the text area, inset by the
  margin and with the scroll bar at its right edge, above the status bar.

  Render pixels are the window's, from its top left. Content pixels are
  the text's: from the text column's left and the top of the text, before
  scrolling. Visual lines are the layout's, counted down the whole text.

  The text an inset holds (see hoatzin.app.insets) is shown through a view
  of the app: the app with the inset's text in place of its own, its
  :origin, the render pixel its text starts at (as scrolled), and :view-h,
  how tall it shows. Everything here answers for either."
  (:require [hoatzin.app.display :as display]
            [hoatzin.app.state :refer [px]]
            [hoatzin.lib.layout :as layout]))

;; ---------------------------------------------------------------- regions

(defn status-height [app]
  (+ (get-in app [:ui :metrics :line-height]) (* 2 (px app (:status-padding app)))))

(defn text-height
  "The height above the status bar: the text, its margins and the scroll bar."
  [app]
  (- (second (:size app)) (status-height app)))

(defn view-height
  "The height the text shows in: the text area, less its margins."
  [app]
  (or (:view-h app) (- (text-height app) (* 2 (px app (:margin app))))))

(defn origin
  "The render pixel [x y] where the text shows: content pixel (0, :scroll)."
  [app]
  (or (:origin app) (let [m (px app (:margin app))] [m m])))

;; ---------------------------------------------------------------- lines

(defn line-top
  "Where visual line `k` starts, in content pixels: after the lines and
  the blocks above it."
  [app k]
  (reduce (fn [y {:keys [line height]}] (if (< line k) (+ y height) (reduced y)))
          (* k (layout/line-height (:layout app)))
          (:block-places app)))

(defn line-at-y
  "The visual line at content pixel `y`, clamped to the text. In a block,
  the line above it."
  [app y]
  (let [L  (:layout app)
        lh (layout/line-height L)
        k  (loop [bs (:block-places app), extra 0]
             (if-let [{:keys [line top height]} (first bs)]
               (cond (< y top)            (Math/floor (/ (double (- y extra)) lh))
                     (< y (+ top height)) line
                     :else                (recur (next bs) (+ extra height)))
               (Math/floor (/ (double (- y extra)) lh))))]
    (-> (long k) (max 0) (min (dec (layout/line-count L))))))

(defn content-height
  "How tall the text is, with its blocks, in pixels."
  [app]
  (let [L (:layout app)]
    (reduce + (* (layout/line-count L) (layout/line-height L))
            (map :height (:block-places app)))))

(defn visible-lines
  "The visual lines [k0, k1) at least partly in view."
  [{:keys [layout scroll] :as app}]
  [(line-at-y app scroll)
   (min (layout/line-count layout) (inc (line-at-y app (+ scroll (view-height app)))))])

(defn point->line
  "The visual line under render pixel (x, y), and x along it: [k x]."
  [app x y]
  (let [[ox oy] (origin app)]
    [(line-at-y app (+ (- y oy) (:scroll app))) (- x ox)]))

;; ---------------------------------------------------------------- positions

(defn caret-place
  "The caret's [x visual-line], minding its affinity at a wrap point. The
  composition moves the caret off any wrap point, so ignore it then."
  [{:keys [layout composition upstream?] :as app}]
  (layout/caret layout (display/view-caret app) (and upstream? (nil? composition))))

(defn caret-or
  "[x visual-line] of `pos`, as drawn if it is the caret."
  [app pos]
  (if (= pos (get-in app [:doc :caret]))
    (caret-place app)
    (layout/caret (:layout app) pos)))
