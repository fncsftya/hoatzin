(ns hoatzin.app.buffers-window
  "The buffers window: the buffers listed over the text, opened by
  `:buffers` and closed by escape. It is on one buffer at a time, which is
  highlighted: the current buffer, as it opens, then the one under the
  pointer, or the one the keys move it to. Up and down move by one, page
  up and page down by a window's worth, and home and end go to the first
  and last. Return, or a click on a buffer, switches to it and closes the
  window. The wheel scrolls it, and so do the keys, to show the buffer
  they move to.

  The app keeps where the window is: :buffers-active, the index of the
  buffer it is on, and :buffers-scroll, how many rows it is scrolled by."
  (:require [hoatzin.app.buffers :as buffers]
            [hoatzin.app.face :refer [ui-width]]
            [hoatzin.app.geometry :refer [status-height]]
            [hoatzin.lib.sdl :as sdl]))

(def ^:private padding "Points inside the buffers window's border." 16)
(def ^:private gap "Points between the buffers window's heading and its rows." 8)
(def ^:private row-padding "Points around each row's text." [2 6])
(def ^:private column-gap "Points between a row's columns." 16)

(defn- row-height
  "A row's height, in points."
  [app]
  (+ (/ (get-in app [:ui :metrics :line-height]) (:density app)) (* 2 (first row-padding))))

(defn visible-rows
  "How many rows fit in the window under its heading."
  [app]
  (let [d  (:density app)
        h  (- (/ (second (:size app)) d) (/ (status-height app) d) (* 2 (:margin app)))
        lh (/ (get-in app [:ui :metrics :line-height]) d)]
    (max 1 (long (Math/floor (/ (- h (* 2 padding) 2 lh gap) (row-height app)))))))

(defn- max-scroll [app n] (max 0 (- n (visible-rows app))))

(defn open
  "Open the buffers window, on the current buffer."
  [app]
  (let [bs (buffers/listing app)
        i  (or (first (keep-indexed #(when (= (:buffer-id app) (:buffer-id %2)) %1) bs)) 0)]
    (assoc app :window :buffers :buffers-active i
           :buffers-scroll (min (max-scroll app (count bs)) (max 0 (- i (dec (visible-rows app)))))
           :dirty? true)))

(defn- scroll-to
  "The app scrolled `n` rows, within the list of `count` buffers."
  [app n count]
  (assoc app :buffers-scroll (-> n (min (max-scroll app count)) (max 0)) :dirty? true))

(defn- move-to
  "The app on buffer `i`, kept within the list, scrolled to show it."
  [app i]
  (let [n    (count (:buffers app))
        i    (-> i (min (dec n)) (max 0))
        rows (visible-rows app)
        top  (:buffers-scroll app 0)
        top  (cond (< i top)            i
                   (>= i (+ top rows))  (inc (- i rows))
                   :else                top)]
    (-> (assoc app :buffers-active i) (scroll-to top n))))

(defn- choose
  "Switch to buffer `i`, closing the window."
  [app now i]
  (if-let [b (get (buffers/listing app) i)]
    (-> app
        (dissoc :buffers-active :buffers-scroll)
        (assoc :window nil)
        (buffers/switch now (:buffer-id b)))
    app))

(defn on-key
  "A key in the buffers window. Other keys do nothing."
  [app now key]
  (let [i    (:buffers-active app 0)
        rows (visible-rows app)]
    (condp = key
      sdl/K-UP       (move-to app (dec i))
      sdl/K-DOWN     (move-to app (inc i))
      sdl/K-PAGEUP   (move-to app (- i rows))
      sdl/K-PAGEDOWN (move-to app (+ i rows))
      sdl/K-HOME     (move-to app 0)
      sdl/K-END      (move-to app (count (:buffers app)))
      sdl/K-RETURN   (choose app now i)
      sdl/K-KP-ENTER (choose app now i)
      app)))

(defn on-wheel [app dy]
  (scroll-to app (- (:buffers-scroll app 0) (long (Math/signum (double dy))))
             (count (:buffers app))))

(defn- row-at
  "The index of the buffer whose row is under render pixel (x, y), or nil."
  [app x y]
  (some (fn [{:keys [node] [rx ry rw rh] :rect}]
          (when (and (:buffer-row node) (<= rx x) (< x (+ rx rw)) (<= ry y) (< y (+ ry rh)))
            (:buffer-row node)))
        (:float-places app)))

(defn on-click
  "A click: on a buffer, switches to it; anywhere else, nothing."
  [app now x y]
  (if-let [i (row-at app x y)] (choose app now i) app))

(defn on-move
  "The pointer moved to render pixel (x, y): over a buffer, the window is
  on it."
  [app x y]
  (let [i (row-at app x y)]
    (if (and i (not= i (:buffers-active app)))
      (assoc app :buffers-active i :dirty? true)
      app)))

(defn window
  "The buffers as a box over the text, inset by the margin: those from
  the scroll on that fit, a row each, with the current buffer marked, each
  buffer's name, [+] if it has unsaved changes, and its file, else its
  directory, dimmed."
  [app]
  (let [m      (:margin app)
        d      (:density app)
        fg     (:foreground app)
        dim    (mapv #(quot (+ (* 2 %1) %2) 3) fg (:window-background app))
        bs     (buffers/listing app)
        names  (buffers/names bs)
        label  (fn [b] (str (names (:buffer-id b)) (when (buffers/unsaved? b) " [+]")))
        name-w (/ (reduce max 0 (map #(ui-width app (label %)) bs)) d)
        mark-w (/ (ui-width app "•") d)
        start  (min (:buffers-scroll app 0) (max-scroll app (count bs)))
        more?  (pos? (max-scroll app (count bs)))]
    {:kind :box
     :style {:position :absolute :left m :top m :right m
             :bottom (+ (/ (status-height app) d) m)
             :padding padding :gap gap :border 1
             :background (:window-background app) :border-color (:ui-border app)}
     :children
     ;; the rows in a box of their own, so that only the heading is a gap above them
     [{:kind :label :text "Buffers" :style {:color fg}}
      {:kind :label :text (if more? "up/down to scroll   esc" "esc")
       :style {:position :absolute :top padding :right padding :color dim}}
      {:kind :box
       :children
       (mapv (fn [i b]
               {:kind :box :buffer-row i
                :style (cond-> {:direction :row :gap column-gap :padding row-padding}
                         (= i (:buffers-active app)) (assoc :background (:ui-highlight app)))
                :children [{:kind :label :text (if (= (:buffer-id b) (:buffer-id app)) "•" "")
                            :style {:width mark-w :color fg}}
                           {:kind :label :text (label b) :style {:width name-w :color fg}}
                           {:kind :label :text (str (or (:path b) (:dir b)))
                            :style {:width 0 :grow 1 :color dim}}]})
             (iterate inc start)
             (take (visible-rows app) (drop start bs)))}]}))
