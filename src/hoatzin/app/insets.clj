(ns hoatzin.app.insets
  "Insets: boxes in the text, as wide as it, each holding text of its own
  to edit, below a header bar that folds and unfolds it. An inset grows
  as its text does, up to `max-rows` lines, and scrolls past that. Modes
  make them (see hoatzin.app.modes): auk mode's sections are insets.

  Each sits below the paragraph holding its mark in the document, as a
  block does (see hoatzin.app.boxes), or with :above? above the first
  one; insets at the same paragraph go in :order. The app's :insets has
  every inset of the current buffer, by id, each
    {:id :order :above? :collapsed? :scroll
     :doc :undo :undo-tail :undo-chain :goal-x :upstream? :composition
     :layout :laid-out :layout-ctx}
  its text being edited as the buffer's is (see `edit-keys`), :scroll how
  far down that is, in pixels, and its layout of the text, from the
  app's :inset-ctx. :inset is the id of the one the caret is in, if any.

  While the caret is in an inset, what edits the text edits the inset's:
  `in-view` runs it on a view of the app (see hoatzin.app.geometry) with
  the inset's text in place of the buffer's, and puts what it did back.
  Up and down cross into and out of an inset at its first and last line,
  and a click in one puts the caret there; a click on its header folds or
  unfolds it."
  (:require [hoatzin.app.display :as display]
            [hoatzin.app.geometry :as geo]
            [hoatzin.app.input.motion :refer [move-on-line]]
            [hoatzin.app.state :refer [px touched]]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.layout :as layout]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.text :as text]))

(def max-rows "The most lines an inset shows before it scrolls." 10)

(def ^:private border "Points: the inset's border." 1)
(def ^:private pad-x "Points between the border and the text, across." 10)
(def ^:private pad-y "Points between the header or border and the text, down." 4)
(def ^:private header-pad "Points above and below the header's text." 3)
(def ^:private thumb-width "Points: the scroll thumb of an inset that scrolls." 3)

(def edit-keys
  "What an inset holds of its text as it is edited: the keys of the app's
  own text that a view replaces."
  [:doc :undo :undo-tail :undo-chain :goal-x :upstream? :composition])

(defn- mark-id [id] [:inset id])

(defn wrap-width
  "How wide an inset's text wraps, for text `width` pixels wide."
  [app width]
  (max 1 (- width (* 2 (+ (px app border) (px app pad-x))))))

(defn active
  "The inset the caret is in, or nil."
  [app]
  (some->> (:inset app) (get (:insets app))))

(defn composing?
  "Whether the text with the caret, the buffer's or an inset's, is being
  composed."
  [app]
  (some? (:composition (or (active app) app))))

;; ---------------------------------------------------------------- the document

(defn- paragraph-of
  "The paragraph of the buffer's text holding `pos`."
  [app pos]
  (first (text/line-at (text/of (get-in app [:doc :text])) pos)))

(defn ordered
  "Every inset, in order down the text, each with :after, the paragraph it
  is below (-1: above the first)."
  [app]
  (let [marks (get-in app [:doc :marks])]
    (->> (vals (:insets app))
         (keep (fn [i]
                 (when-let [pos (get marks (mark-id (:id i)))]
                   (assoc i :after (if (:above? i) -1 (paragraph-of app pos))))))
         (sort-by (juxt :after :order)))))

(defn snapshot
  "What the insets hold, as a file has it: [[after text] ...] in order
  down the text."
  [app]
  (mapv (fn [i] [(:after i) (str (get-in i [:doc :text]))]) (ordered app)))

(defn- fresh
  "A new inset numbered `id` of string `s`, the caret at its start."
  [id order above? s]
  {:id id :order order :above? above? :scroll 0 :doc (ed/doc s)})

(defn load-all
  "The app with insets `specs`, [{:after k :text s}] in order down the
  text (:after -1 above the first paragraph), in place of any it had."
  [app specs]
  (let [old (keys (:insets app))
        doc (reduce #(ed/unmark %1 (mark-id %2)) (:doc app) old)
        t   (text/of (:text doc))
        n   (text/line-count t)
        [doc insets next-id]
        (reduce (fn [[doc insets id] [order {:keys [after text]}]]
                  (let [k      (min after (dec n))
                        above? (neg? k)
                        pos    (if above? 0 (+ (text/line-start t k) (count (text/line t k))))]
                    [(ed/mark doc (mark-id id) pos)
                     (assoc insets id (fresh id order above? text))
                     (inc id)]))
                [doc {} (:next-inset-id app 0)]
                (map-indexed vector specs))]
    (-> app
        (assoc :doc doc :insets insets :next-inset-id next-id :dirty? true)
        (dissoc :inset))))

;; ---------------------------------------------------------------- the caret

(defn- collapse
  "`doc` with its selection, if any, given up, the caret where it is."
  [doc]
  (ed/move doc (:caret doc)))

(defn leave
  "The caret out of the inset it is in, back in the buffer's text where
  it was; the inset's selection goes."
  [app now]
  (if-let [id (:inset app)]
    (-> app
        (update-in [:insets id :doc] collapse)
        (dissoc :inset)
        (touched now))
    app))

(defn- enter
  "The caret into inset `id`, where it was there: the buffer's selection
  goes."
  [app now id]
  (-> (leave app now)
      (update :doc collapse)
      (assoc :inset id)
      (touched now)))

(defn add
  "A new inset of string `s` (\"\" if not given), and the caret into it:
  below the inset the caret is in, else the paragraph it is in, after
  any other insets there."
  ([app now] (add app now ""))
  ([app now s]
   (let [id  (:next-inset-id app 0)
         cur (active app)
         pos (if cur
               (get-in app [:doc :marks (mark-id (:id cur))])
               (let [t (text/of (get-in app [:doc :text]))
                     [_ start line] (text/line-at t (get-in app [:doc :caret]))]
                 (+ start (count line))))
         above? (boolean (:above? cur))
         after  (if above? -1 (paragraph-of app pos))
         orders (->> (ordered app) (filter #(= after (:after %))) (map :order))
         order  (if cur
                  (if-let [next (seq (filter #(> % (:order cur)) orders))]
                    (/ (+ (:order cur) (reduce min next)) 2.0)
                    (inc (:order cur)))
                  (inc (reduce max -1 orders)))]
     (-> app
         (update :doc ed/mark (mark-id id) pos)
         (assoc-in [:insets id] (fresh id order above? s))
         (assoc :next-inset-id (inc id))
         (enter now id)))))

(defn remove-inset
  "The app without inset `id`; if the caret was in it, it is back in the
  buffer's text."
  [app now id]
  (-> (if (= id (:inset app)) (leave app now) app)
      (update :doc ed/unmark (mark-id id))
      (update :insets dissoc id)
      (touched now)))

(defn toggle
  "Fold inset `id` to its header, or unfold it. Folding the inset the
  caret is in takes the caret out."
  [app now id]
  (let [app (if (and (= id (:inset app)) (not (get-in app [:insets id :collapsed?])))
              (leave app now)
              app)]
    (-> app (update-in [:insets id :collapsed?] not) (assoc :dirty? true))))

;; ---------------------------------------------------------------- layout

(defn sync-layouts
  "Lay out again the text of each unfolded inset that has changed since
  it last was, or every one, if the inset context has; and keep each
  one's scroll within its text."
  [app]
  (let [ctx (:inset-ctx app)]
    (if (or (nil? ctx) (empty? (:insets app)))
      app
      (let [changed? (volatile! false)
            insets (update-vals
                    (:insets app)
                    (fn [i]
                      (if (or (:collapsed? i)
                              (and (identical? ctx (:layout-ctx i))
                                   (display/same-display? (display/display-key i) (:laid-out i))))
                        i
                        (let [L (layout/layout ctx (display/display-text i))
                              lh (layout/line-height L)
                              most (* lh (max 0 (- (layout/line-count L) max-rows)))]
                          (vreset! changed? true)
                          (assoc i :layout L :laid-out (display/display-key i) :layout-ctx ctx
                                 :scroll (min (:scroll i 0) most))))))]
        (cond-> (assoc app :insets insets) @changed? (assoc :dirty? true))))))

(defn- rows [inset] (min max-rows (layout/line-count (:layout inset))))

(defn- dim [app] (mapv #(quot (+ (* 2 %1) %2) 3) (:foreground app) (:window-background app)))

(defn- node
  "Inset `i` as a box (see hoatzin.lib.ui): its header, saying `title`,
  and unless it is folded, room for as many lines of its text as it
  shows."
  [app i title]
  (let [folded? (or (:collapsed? i) (nil? (:layout i)))
        first-line (first (text/lines (text/of (get-in i [:doc :text])) 0 1))
        n (when (:layout i) (layout/line-count (:layout i)))]
    {:kind :box
     :style {:border border :border-color (:ui-border app) :background (:background app)}
     :children
     (cond-> [{:kind :box :inset-header (:id i)
               :style {:direction :row :gap 8 :align :center
                       :padding [header-pad (- pad-x border)] :background (:window-background app)}
               :children (cond-> [{:kind :label :text (if folded? "▸" "▾") :style {:color (dim app)}}
                                  {:kind :label
                                   :text (if (and folded? (seq first-line)) (str title ": " first-line) title)
                                   :style {:width 0 :grow 1 :color (:foreground app)}}]
                           (and folded? n) (conj {:kind :label :text (str n (if (= 1 n) " line" " lines"))
                                                  :style {:color (dim app)}}))}]
       (not folded?)
       (conj {:kind :box :inset-body (:id i)
              :style {:padding [pad-y pad-x]
                      :height (+ (/ (* (rows i) (layout/line-height (:layout i))) (:density app))
                                 (* 2 pad-y))}}))}))

(defn blocks
  "Every inset as a block to place (see hoatzin.app.boxes/place-blocks):
  {:id :inset :node :pos :line :order}, :line the visual line it is
  below (-1: above the first), its header saying `title`."
  [app title]
  (let [L (:layout app)
        marks (get-in app [:doc :marks])]
    (keep (fn [i]
            (when-let [pos (get marks (mark-id (:id i)))]
              (let [pos (display/shown-pos app pos)]
                {:id (mark-id (:id i)) :inset (:id i) :node (node app i title) :pos pos
                 :order (:order i)
                 :line (if (:above? i) (dec (layout/first-line L pos)) (layout/last-line L pos))})))
          (vals (:insets app)))))

(defn parts
  "Where a placed inset's header and text are, from its placed boxes
  `placed`: {:header rect :body rect :text rect}, :body the box the text
  is in and :text inside its padding; no :body while it is folded."
  [placed]
  (let [find (fn [k] (some #(when (get-in % [:node k]) %) placed))]
    (merge {:header (:rect (find :inset-header))}
           (when-let [b (find :inset-body)] {:body (:rect b) :text (:content b)}))))

(defn place-of
  "Inset `id` as it is placed, from the app's :block-places, or nil."
  [app id]
  (some #(when (= id (:inset %)) %) (:block-places app)))

;; ---------------------------------------------------------------- the view

(defn view
  "The app as inset `id` shows it: with the inset's text in place of the
  buffer's, scrolled as the inset is, and shown where the inset's text is
  (see hoatzin.app.geometry); nil while it is folded."
  [app id]
  (when-let [[tx ty tw th] (:text (place-of app id))]
    (let [i (get-in app [:insets id])
          [ox oy] (geo/origin app)]
      (-> (apply dissoc app :inset edit-keys)
          (merge (select-keys i edit-keys))
          (assoc :layout (:layout i) :scroll (:scroll i 0) :block-places []
                 :origin [(+ ox tx) (+ oy (- ty (:scroll app)))] :view-h th :view-w tw)))))

(def ^:private view-keys
  "What a view replaces besides the text being edited."
  [:layout :scroll :block-places :inset :insets])

(defn- unview
  "The app after view `v` of inset `id` became `v`: the inset's text as
  `v` has it, everything else as it was, but for what `v` changed."
  [app id v]
  (let [ks (concat edit-keys view-keys)]
    (-> (apply dissoc v :origin :view-h :view-w ks)
        (merge (select-keys app ks))
        (update-in [:insets id] #(merge (apply dissoc % edit-keys) (select-keys v edit-keys))))))

(defn in-view
  "`f` of the text with the caret: of a view of the inset it is in (see
  `view`), else of the app."
  [app f]
  (if-let [id (:inset app)]
    (if-let [v (view app id)]
      (unview app id (f v))
      (f (dissoc app :inset)))
    (f app)))

(defn caret-top
  "The top of the caret's line, in the buffer's content pixels."
  [app]
  (let [[_ k] (geo/caret-place app)]
    (if-let [v (some->> (:inset app) (view app))]
      (let [[_ vk] (geo/caret-place v)
            [_ ty] (:text (place-of app (:inset app)))]
        (+ ty (- (geo/line-top v vk) (:scroll v))))
      (geo/line-top app k))))

(defn follow
  "Scroll the inset the caret is in, if any, just enough to show its line."
  [app]
  (if-let [v (some->> (:inset app) (view app))]
    (let [lh (layout/line-height (:layout v))
          [_ k] (geo/caret-place v)
          top (* k lh)
          s (:scroll v)
          vh (geo/view-height v)]
      (assoc-in app [:insets (:inset app) :scroll]
                (cond (< top s) top
                      (> (+ top lh) (+ s vh)) (- (+ top lh) vh)
                      :else s)))
    app))

;; ---------------------------------------------------------------- crossing

(defn- enter-at
  "The caret into inset `b`, as placed, on its first (or `last?` last)
  line, nearest the buffer's content x `x`."
  [app now b last? x]
  (let [id (:inset b)
        [tx] (:text b)
        L (get-in app [:insets id :layout])
        k (if last? (dec (layout/line-count L)) 0)
        app (enter app now id)]
    (in-view app #(move-on-line % now false k (layout/position-at L k (- x tx)) (- x tx)))))

(defn- open? [b] (and (:inset b) (:text b)))

(defn cross
  "Up or down `key`, without modifiers, from the last line above an
  unfolded inset into it, from its first or last line out of it, or into
  the next inset at the same place; nil if the key doesn't cross."
  [app now key mod]
  (when (and (zero? (bit-and mod (bit-or sdl/KMOD-SHIFT sdl/KMOD-GUI sdl/KMOD-CTRL)))
             (or (= key sdl/K-UP) (= key sdl/K-DOWN)))
    (let [down? (= key sdl/K-DOWN)
          bs    (:block-places app)]
      (if-let [id (:inset app)]
        (when-let [v (view app id)]
          (let [b (place-of app id)
                [x k] (geo/caret-place v)
                [tx] (:text b)
                x (+ tx (or (:goal-x v) x))
                n (layout/line-count (:layout v))
                same (filter #(and (open? %) (= (:line %) (:line b))) bs)
                [before after] (split-with #(not= id (:inset %)) same)]
            (cond
              (and down? (= k (dec n)))
              (if-let [nb (second after)]
                (enter-at app now nb false x)
                (when (< (inc (:line b)) (layout/line-count (:layout app)))
                  (let [app (leave app now), k2 (inc (:line b))]
                    (move-on-line app now false k2 (layout/position-at (:layout app) k2 x) x))))

              (and (not down?) (zero? k))
              (if-let [pb (last before)]
                (enter-at app now pb true x)
                (when (>= (:line b) 0)
                  (let [app (leave app now), k2 (:line b)]
                    (move-on-line app now false k2 (layout/position-at (:layout app) k2 x) x)))))))
        (let [[x k] (geo/caret-place app)
              x (or (:goal-x app) x)]
          (if down?
            (some-> (first (filter #(and (open? %) (= k (:line %))) bs)) (as-> b (enter-at app now b false x)))
            (some-> (last (filter #(and (open? %) (= (dec k) (:line %))) bs)) (as-> b (enter-at app now b true x)))))))))

;; ---------------------------------------------------------------- the pointer

(defn- inside? [[rx ry rw rh] x y] (and rx (<= rx x) (< x (+ rx rw)) (<= ry y) (< y (+ ry rh))))

(defn hit
  "The inset under render pixel (x, y), where the text shows, as
  {:id :part}, :part :header or :body; or nil."
  [app x y]
  (let [m (px app (:margin app))
        cx (- x m)
        cy (+ (- y m) (:scroll app))]
    (when (and (>= y m) (< y (+ m (geo/view-height app))))
      (some (fn [b]
              (when (:inset b)
                (cond (inside? (:header b) cx cy) {:id (:inset b) :part :header}
                      (inside? (:body b) cx cy)   {:id (:inset b) :part :body})))
            (:block-places app)))))

(defn click
  "A click on inset `hit` (see `hit`): on its header, fold or unfold it;
  in its text, the caret there, by `on-click` of the view, which takes
  the app, `now` and the click."
  [app now {:keys [id part]} on-click]
  (if (= :header part)
    (toggle app now id)
    (in-view (enter app now id) on-click)))

(defn- most-scroll [i]
  (let [L (:layout i)]
    (* (layout/line-height L) (max 0 (- (layout/line-count L) max-rows)))))

(defn on-wheel
  "The wheel over render pixel (x, y): the inset there scrolls, if it has
  more lines than it shows; else nil."
  [app x y dy]
  (when-let [{:keys [id part]} (and x (hit app x y))]
    (let [i (get-in app [:insets id])]
      (when (and (= :body part) (pos? (most-scroll i)))
        (let [lh (layout/line-height (:layout i))
              s  (- (:scroll i 0) (* dy (:wheel-lines app) lh))]
          (-> app
              (assoc-in [:insets id :scroll] (long (max 0 (min (most-scroll i) (Math/round (double s))))))
              (assoc :dirty? true)))))))

(defn thumb
  "The scroll thumb of inset `i` placed as `b`, in content pixels, or nil
  if it shows all its lines: [x y w h], at the right of its text."
  [app i b]
  (when-let [[bx _ bw] (:body b)]
    (let [most (most-scroll i)]
      (when (pos? most)
        (let [[_ ty _ th] (:text b)
              lh (layout/line-height (:layout i))
              total (* lh (layout/line-count (:layout i)))
              h (max (px app 12) (quot (* th th) total))
              w (px app thumb-width)]
          [(- (+ bx bw) (quot (+ (px app pad-x) w) 2))
           (+ ty (long (Math/round (/ (* (- th h) (double (:scroll i 0))) most))))
           w h])))))
