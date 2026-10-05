(ns hoatzin.app.boxes
  "Boxes (see hoatzin.lib.ui) shown with the text: blocks, in the text
  below a paragraph, and floats, above everything. Adding them, and where
  they are placed."
  (:require [hoatzin.app.command :as command]
            [hoatzin.app.display :refer [shown-pos]]
            [hoatzin.app.face :refer [ui-context]]
            [hoatzin.app.settings-window :as settings-window]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.layout :as layout]
            [hoatzin.lib.ui :as ui]))

(defn add-block
  "Show box `node` (see hoatzin.lib.ui) in the text, below the paragraph
  holding position `pos` and as wide as the text, which makes room for it.
  It keeps to that paragraph as the text is edited. Replaces any block
  `id` was."
  [app id pos node]
  (-> app
      (update :doc ed/mark id pos)
      (assoc-in [:blocks id] node)
      (assoc :dirty? true)))

(defn remove-block [app id]
  (-> app (update :doc ed/unmark id) (update :blocks dissoc id) (assoc :dirty? true)))

(defn set-floats
  "Show boxes `nodes` above everything, placed in the window: those in
  flow down its left edge, absolute ones where they say. Later ones are
  drawn over earlier ones."
  [app nodes]
  (assoc app :floats (vec nodes) :dirty? true))

(defn set-ui-value
  "Set what the interactive box `id` holds."
  [app id value]
  (-> app (assoc-in [:ui-values id] value) (assoc :dirty? true)))

;; ---------------------------------------------------------------- placing

(defn place-blocks
  "Every block with a mark in the document, in order down the text, as
  {:id :pos :line :top :height :placed}: below visual line :line, the last
  of the paragraph holding its mark, as wide as the text. :top and :placed
  (from hoatzin.lib.ui/place) are in content pixels: from the text column's
  left and the top of the text as scrolled."
  [app]
  (let [L      (:layout app)
        lh     (layout/line-height L)
        marks  (get-in app [:doc :marks])
        width  (get-in app [:ctx :width])
        ctx    (ui-context app)
        blocks (->> (:blocks app)
                    (keep (fn [[id node]]
                            (when-let [pos (get marks id)]
                              (let [pos (shown-pos app pos)]
                                {:id id :node node :pos pos :line (layout/last-line L pos)}))))
                    (sort-by (juxt :line :pos)))]
    (loop [bs blocks, extra 0, out []]
      (if-let [{:keys [node line] :as b} (first bs)]
        (let [h (second (ui/measure ctx node))
              top (+ (* (inc line) lh) extra)]
          (recur (next bs) (+ extra h)
                 (conj out (-> (dissoc b :node)
                               (assoc :top top :height h
                                      :placed (ui/place ctx node [0 top width h]))))))
        out))))

(defn place-floats
  "The floats placed in the window, in render pixels: in a box as big as
  it, which lays out those in flow down its left edge. The open window, if
  any, then the command line's hints, if any, go on top."
  [app]
  (if-let [floats (seq (cond-> (:floats app)
                         (= :settings (:window app)) (conj (settings-window/window app))
                         (command/hints app)         (conj (command/hints app))))]
    (let [[w h] (:size app)]
      (subvec (ui/place (ui-context app)
                        {:kind :box :style {:align :start} :children (vec floats)}
                        [0 0 w h])
              1))
    []))
