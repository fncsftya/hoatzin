(ns hoatzin.app.buffers-window
  "The buffers window: the buffers listed over the text, opened by
  `:buffers` and closed by escape. It is on one buffer at a time, which is
  highlighted: the current buffer, as it opens, then the one under the
  pointer, or the one the keys move it to. Up and down move by one, page
  up and page down by a window's worth, and home and end go to the first
  and last. Return, or a click on a buffer, switches to it and closes the
  window. The wheel scrolls it, and so do the keys, to show the buffer
  they move to. On a buffer,
    k   closes it, once asked if it has changes that would be lost, or is
        not the scratch buffer and has no file
    p   previews its text, in place of the list, until escape
    t   tags it: what is typed is the tag, shown at the right of its row
        in a pastel colour of its own, until return keeps it (with
        nothing typed, taking any tag away) or escape gives it up
  Each buffer shows its file, or *unsaved* if it has none.

  The app keeps where the window is: :buffers-active, the index of the
  buffer it is on, :buffers-scroll, how many rows it is scrolled by,
  :buffers-preview, the id of the buffer previewed, if any, and
  :buffers-tagging, {:id :text}, the buffer being tagged and the tag
  typed so far, if any."
  (:require [clojure.string :as str]
            [hoatzin.app.buffers :as buffers]
            [hoatzin.app.confirm :as confirm]
            [hoatzin.app.face :refer [ui-width]]
            [hoatzin.app.geometry :refer [status-height]]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.text :as text]))

(def ^:private padding "Points inside the buffers window's border." 16)
(def ^:private gap "Points between the buffers window's heading and its rows." 8)
(def ^:private row-padding "Points around each row's text." [2 6])
(def ^:private column-gap "Points between a row's columns." 16)
(def ^:private tag-padding "Points around a tag's text." [0 6])
(def ^:private tag-max "The most characters a tag has." 16)
(def ^:private tag-text "The colour of a tag's text, on its pastel." [40 40 50])

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

;; ---------------------------------------------------------------- closing, previewing, tagging

(defn- active
  "The buffer the window is on, or nil."
  [app]
  (get (buffers/listing app) (:buffers-active app 0)))

(defn- close-active
  "Close buffer `id`, the window staying open, on the buffer that takes
  its place in the list."
  [app now id]
  (let [app (buffers/close-buffer app now id)]
    (-> (move-to app (:buffers-active app 0)) (assoc :window :buffers))))

(defn- ask-close
  "Close the buffer the window is on, once asked if closing it could
  lose what it holds: its unsaved changes, or text that isn't the
  scratch buffer's and is in no file."
  [app now]
  (if-let [b (active app)]
    (let [id    (:buffer-id b)
          name  ((buffers/names (buffers/listing app)) id)
          risky (or (buffers/unsaved? b) (and (nil? (:path b)) (not (:scratch? b))))]
      (if risky
        (confirm/ask app (str "Close \"" name "\"" (if (:path b) ", with unsaved changes" ", which has no file")
                              "? (y/n)")
                     (fn [app now] (close-active app now id)))
        (close-active app now id)))
    app))

(defn- pastel
  "A pastel colour, [r g b], of hue `h` (0 to 1)."
  [h]
  (let [s 0.7, l 0.82
        q (+ l (* s (- 1 l)))
        p (- (* 2 l) q)
        f (fn [t]
            (let [t (mod t 1.0)]
              (cond (< t (/ 1 6.0)) (+ p (* (- q p) 6 t))
                    (< t 0.5)       q
                    (< t (/ 2 3.0)) (+ p (* (- q p) (- (/ 2 3.0) t) 6))
                    :else           p)))]
    (mapv #(long (Math/round (* 255 (f %)))) [(+ h (/ 1 3.0)) h (- h (/ 1 3.0))])))

(defn- end-tagging
  "Keep the tag typed, or with `keep?` false give it up."
  [app keep?]
  (let [{:keys [id text]} (:buffers-tagging app)
        app (assoc (dissoc app :buffers-tagging) :dirty? true)]
    (cond
      (not keep?)       app
      (str/blank? text) (buffers/update-buffer app id dissoc :tag :tag-color)
      :else             (buffers/update-buffer app id assoc :tag (str/trim text)
                                               :tag-color (pastel ((:rand-fn app rand)))))))

(defn on-text
  "Typed text in the buffers window: onto the tag being typed, if one
  is, else k, p and t close, preview and tag the buffer the window is on."
  [app now text]
  (cond
    (:buffers-tagging app)
    (-> app
        (update-in [:buffers-tagging :text] #(subs (str % text) 0 (min tag-max (count (str % text)))))
        (assoc :dirty? true))

    (:buffers-preview app) app

    :else
    (case text
      "k" (ask-close app now)
      "p" (if-let [b (active app)] (assoc app :buffers-preview (:buffer-id b) :dirty? true) app)
      "t" (if-let [b (active app)]
            (assoc app :buffers-tagging {:id (:buffer-id b) :text ""} :dirty? true)
            app)
      app)))

(defn on-escape
  "Escape in the buffers window: ends the preview, or gives up the tag
  being typed; nil if there is neither, to close the window."
  [app]
  (cond (:buffers-tagging app) (end-tagging app false)
        (:buffers-preview app) (assoc (dissoc app :buffers-preview) :dirty? true)))

(defn on-key
  "A key in the buffers window. Other keys do nothing."
  [app now key]
  (let [i    (:buffers-active app 0)
        rows (visible-rows app)]
    (cond
      (:buffers-tagging app)
      (condp = key
        sdl/K-RETURN    (end-tagging app true)
        sdl/K-KP-ENTER  (end-tagging app true)
        sdl/K-BACKSPACE (-> app (update-in [:buffers-tagging :text] #(subs % 0 (max 0 (dec (count %)))))
                            (assoc :dirty? true))
        app)

      (:buffers-preview app) app

      :else
      (condp = key
        sdl/K-UP       (move-to app (dec i))
        sdl/K-DOWN     (move-to app (inc i))
        sdl/K-PAGEUP   (move-to app (- i rows))
        sdl/K-PAGEDOWN (move-to app (+ i rows))
        sdl/K-HOME     (move-to app 0)
        sdl/K-END      (move-to app (count (:buffers app)))
        sdl/K-RETURN   (choose app now i)
        sdl/K-KP-ENTER (choose app now i)
        app))))

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
  "A click: on a buffer, switches to it; anywhere else, nothing. While a
  tag is typed, or a buffer previewed, nothing."
  [app now x y]
  (if-let [i (and (not (:buffers-tagging app)) (not (:buffers-preview app)) (row-at app x y))]
    (choose app now i)
    app))

(defn on-move
  "The pointer moved to render pixel (x, y): over a buffer, the window is
  on it, unless a tag is being typed."
  [app x y]
  (let [i (row-at app x y)]
    (if (and i (not= i (:buffers-active app)) (not (:buffers-tagging app)) (not (:buffers-preview app)))
      (assoc app :buffers-active i :dirty? true)
      app)))

(defn- frame
  "The window's box, over the text and inset by the margin, holding
  `children`."
  [app children]
  (let [m (:margin app)
        d (:density app)]
    {:kind :box
     :style {:position :absolute :left m :top m :right m
             :bottom (+ (/ (status-height app) d) m)
             :padding padding :gap gap :border 1
             :background (:window-background app) :border-color (:ui-border app)}
     :children children}))

(defn- dimmed [app] (mapv #(quot (+ (* 2 %1) %2) 3) (:foreground app) (:window-background app)))

(defn- wrapped
  "Line `s` as lines of no more than `n` characters, broken after a
  space where there is one."
  [s n]
  (lazy-seq
   (if (<= (count s) n)
     [s]
     (let [space (str/last-index-of (subs s 0 (inc n)) " ")
           cut   (if (and space (pos? space)) (inc space) n)]
       (cons (subs s 0 cut) (wrapped (subs s cut) n))))))

(defn- preview
  "The window previewing buffer `b`: its name, and as much of its text
  as fits, wrapped to the window's width."
  [app b]
  (let [d     (:density app)
        fg    (:foreground app)
        [w h] (map #(/ % d) (:size app))
        lh    (/ (get-in app [:ui :metrics :line-height]) d)
        inner (- w (* 2 (:margin app)) (* 2 padding) 2)
        tall  (- h (/ (status-height app) d) (* 2 (:margin app)) (* 2 padding) 2 lh gap)
        rows  (max 1 (long (Math/floor (/ tall lh))))
        chars (max 1 (long (Math/floor (/ inner (max 1 (/ (ui-width app "M") d))))))
        t     (text/of (get-in b [:doc :text]))
        ;; only the paragraphs that could show, however long the text
        lines (->> (range (min rows (text/line-count t)))
                   (map #(str/replace (text/line t %) "\t" "    "))
                   (mapcat #(wrapped % chars))
                   (take rows))]
    (frame app
           [{:kind :label :text (str "Preview: " ((buffers/names (buffers/listing app)) (:buffer-id b)))
             :style {:color fg}}
            {:kind :label :text "esc" :style {:position :absolute :top padding :right padding :color (dimmed app)}}
            {:kind :box
             :children (mapv (fn [l] {:kind :label :text l :style {:color fg}})
                             (if (= [""] lines) ["(empty)"] lines))}])))

(defn- tag-node
  "Buffer `b`'s tag, as the window shows it: the one being typed, with a
  bar after it and a border, else its own on its pastel; nil if it has
  none."
  [app b]
  (let [{:keys [id text]} (:buffers-tagging app)]
    (cond
      (= id (:buffer-id b))
      {:kind :label :text (str text "|")
       :style {:color (:foreground app) :padding tag-padding :border 1 :border-color (:ui-focus app)}}

      (:tag b)
      {:kind :label :text (:tag b)
       :style {:color tag-text :background (:tag-color b) :padding tag-padding}})))

(declare list-window)

(defn window
  "The buffers as a box over the text, inset by the margin: those from
  the scroll on that fit, a row each, with the current buffer marked, each
  buffer's name, [+] if it has unsaved changes, its file, else *unsaved*,
  dimmed, and its tag. Previewing a buffer, that in place of the list."
  [app]
  (if-let [b (some->> (:buffers-preview app)
                      (#(some (fn [b] (when (= % (:buffer-id b)) b)) (buffers/listing app))))]
    (preview app b)
    (list-window app)))

(defn- list-window
  "The buffers, listed: see `window`."
  [app]
  (let [d      (:density app)
        fg     (:foreground app)
        dim    (dimmed app)
        bs     (buffers/listing app)
        names  (buffers/names bs)
        label  (fn [b] (str (names (:buffer-id b)) (when (buffers/unsaved? b) " [+]")))
        name-w (/ (reduce max 0 (map #(ui-width app (label %)) bs)) d)
        mark-w (/ (ui-width app "•") d)
        start  (min (:buffers-scroll app 0) (max-scroll app (count bs)))
        more?  (pos? (max-scroll app (count bs)))]
    (frame
     app
     ;; the rows in a box of their own, so that only the heading is a gap above them
     [{:kind :label :text "Buffers" :style {:color fg}}
      {:kind :label :text (cond (:buffers-tagging app) "return keeps   esc"
                                more? "up/down   k close  p preview  t tag   esc"
                                :else "k close  p preview  t tag   esc")
       :style {:position :absolute :top padding :right padding :color dim}}
      {:kind :box
       :children
       (mapv (fn [i b]
               {:kind :box :buffer-row i
                :style (cond-> {:direction :row :gap column-gap :padding row-padding}
                         (= i (:buffers-active app)) (assoc :background (:ui-highlight app)))
                :children (cond-> [{:kind :label :text (if (= (:buffer-id b) (:buffer-id app)) "•" "")
                                    :style {:width mark-w :color fg}}
                                   {:kind :label :text (label b) :style {:width name-w :color fg}}
                                   {:kind :label :text (or (:path b) "*unsaved*")
                                    :style {:width 0 :grow 1 :color dim}}]
                            (tag-node app b) (conj (tag-node app b)))})
             (iterate inc start)
             (take (visible-rows app) (drop start bs)))}])))
