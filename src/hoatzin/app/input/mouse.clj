(ns hoatzin.app.input.mouse
  "Clicking and dragging in the text. A click moves the caret, shift-click
  extends the selection, and a double click selects a word; dragging
  extends the selection (by words, after a double click), scrolling while
  it is held above or below the text."
  (:require [hoatzin.app.geometry :refer [point->line view-height]]
            [hoatzin.app.input.motion :refer [move-on-line select-range]]
            [hoatzin.app.state :refer [px]]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.layout :as layout]
            [hoatzin.lib.sdl :as sdl]))

(defn- word-at
  "The [lo hi] word (or whitespace) under the point, or nil on an empty line."
  [app k x]
  (when-let [i (layout/char-at (:layout app) k x)]
    (ed/word-range (get-in app [:doc :text]) i)))

(defn on-click [app now x0 y mod clicks]
  (let [[k x] (point->line app x0 y)
        word (when (>= clicks 2) (word-at app k x))]
    (-> (if-let [[lo hi] word]
          (select-range app now lo hi)
          (move-on-line app now (pos? (bit-and mod sdl/KMOD-SHIFT))
                        k (layout/position-at (:layout app) k x)))
        (assoc :dragging? true :drag-word word :drag-point [x0 y]))))

(defn on-drag [app now x0 y]
  (let [[k x] (point->line app x0 y)
        pos (layout/position-at (:layout app) k x)
        app (assoc app :drag-point [x0 y])]
    (cond
      (not (:dragging? app)) app
      ;; After a double click, the selection grows a word at a time and
      ;; always keeps the word first clicked.
      (:drag-word app)
      (let [[lo hi] (:drag-word app)
            [wlo whi] (or (word-at app k x) [pos pos])]
        (if (< wlo lo)
          (select-range app now hi wlo)
          (select-range app now lo (max hi whi))))
      :else (move-on-line app now true k pos))))

(defn on-release
  "The button is up: what it was dragging, the text's selection or the
  scroll bar's thumb, is let go."
  [app]
  (cond-> (dissoc app :dragging? :drag-word :drag-point :grab)
    (:grab app) (assoc :dirty? true)))

(defn autoscrolling?
  "Whether a text drag is held above or below the text, which scrolls."
  [app]
  (when-let [[_ y] (and (:dragging? app) (:drag-point app))]
    (let [m (px app (:margin app))]
      (or (< y m) (>= y (+ m (view-height app)))))))

(defn autoscroll
  "Drag again at the held point: it is further along the text now that the
  last step scrolled, so the selection grows and scrolls on."
  [app now]
  (if (autoscrolling? app)
    (let [[x y] (:drag-point app)
          app2  (on-drag app now x y)]
      (if (= (:doc app2) (:doc app)) app app2))
    app))
