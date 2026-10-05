(ns hoatzin.app.hints
  "A box of hints across the window, just above the status bar: the
  command line's, and the choices a mode asks for (see
  hoatzin.app.choose)."
  (:require [hoatzin.app.face :refer [ui-width]]
            [hoatzin.app.geometry :refer [status-height]]
            [hoatzin.app.state :refer [px]]))

(def ^:private hint-gap "Points between the columns of hints." 16)
(def ^:private hint-padding "Points above and below the hints." 4)
(def ^:private hint-space "Points between the hints and the status bar." 4)
(def ^:private hint-rows "The most rows of hints shown." 2)

(defn- columns
  "`items` in columns, read down each and then across, filling no more
  than `hint-rows` rows: on one row while they fit across `avail` pixels
  in columns `col-w` wide and `gap` apart, else on as many columns as fit,
  leaving out those that don't."
  [items col-w gap avail]
  (let [fit  (max 1 (quot (+ avail gap) (+ col-w gap)))
        rows (if (<= (count items) fit) 1 hint-rows)]
    (mapv vec (partition-all rows (take (* rows fit) items)))))

(defn box
  "A float across the window above the status bar, holding `items`, each
  {:text s :node n}: the node is shown, and the text is what is measured
  to set the width of the columns."
  [app items]
  (let [d     (:density app)
        [w]   (:size app)
        m     (px app (:margin app))
        col-w (reduce max (map #(ui-width app (:text %)) items))
        cols  (columns items col-w (px app hint-gap) (- w (* 2 m)))]
    ;; The side padding, with the border, is the margin: the hints line
    ;; up with the status bar's text.
    {:kind :box
     :style {:position :absolute :left 0 :right 0
             :bottom (+ (/ (status-height app) d) hint-space)
             :direction :row :gap hint-gap :border 1
             :padding [hint-padding (- (:margin app) 1)]
             :background (:window-background app) :border-color (:ui-border app)}
     :children (mapv (fn [col] {:kind :box :style {:width (/ col-w d)}
                                :children (mapv :node col)})
                     cols)}))
