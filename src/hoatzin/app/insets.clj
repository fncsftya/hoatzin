(ns hoatzin.app.insets
  "Insets: text of its own inside the text, as wide as it. An inset is of
  a :kind:
    :section    (the default) a box, below a header bar that names it,
                with its :title, else what modes call it, and folds and
                unfolds it; its text may hold insets too, to any depth. It
                grows as its text does, up to `max-rows` lines' height, and
                scrolls past that
    :list       a bullet before each paragraph, its items
    :checklist  a box before each, ticked or not, as its :checked has it
  Modes make them (see hoatzin.app.modes): auk mode's sections and lists
  are insets.

  A text, the buffer's or an inset's, is a level: a map of `text-keys`,
  the buffer's at the top of the app. A level's :insets are the insets in
  its text, by id; :inset the id of the one the caret is in, if any, and
  :next-inset-id the id the next is to have. Each inset is a level itself,
  and also
    {:id :order :above? :kind :title :collapsed? :checked :checked-text
     :renaming :layout :laid-out :layout-ctx :block-places}
  It sits below the paragraph of its level's text holding its mark, as a
  block does (see hoatzin.app.boxes), or with :above? above the first
  one; insets at the same paragraph go in :order. Its text is laid out
  with a context of the app's :inset-ctxs, by width, and its own insets
  placed in it as its :block-places. A checklist's :checked are of its
  text as it was at :checked-text, and follow it as it is edited.

  The caret is in the text at the end of the chain of :inset ids down from
  the buffer's, or, if the inset there is folded, over that inset's
  header. What edits the text edits that one: `in-view` runs it on a view
  of the app (see hoatzin.app.geometry), the app with the inset's level in
  place of the buffer's, and puts what it did back. Up and down cross into
  and out of an inset at its first and last line, and onto and off a
  folded one; a click in one puts the caret there, and one on its header
  folds or unfolds it. Folding the section the caret is in leaves the
  caret over it; unfolding the one it is over puts it inside."
  (:require [clojure.string :as str]
            [hoatzin.app.display :as display]
            [hoatzin.app.geometry :as geo]
            [hoatzin.app.history :as history]
            [hoatzin.app.input.motion :refer [move-on-line]]
            [hoatzin.app.state :refer [px touched]]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.layout :as layout]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.text :as text]))

(def max-rows "The most lines' height a section shows before it scrolls." 10)

(def ^:private border "Points: a section's border." 1)
(def ^:private pad-x "Points between a section's border and its text, across." 10)
(def ^:private pad-y "Points between a section's header or border and its text, down." 4)
(def ^:private header-pad "Points above and below a section's header's text." 3)
(def ^:private thumb-width "Points: the scroll thumb of a section that scrolls." 3)
(def gutter "Points: a list's room for its bullets or boxes, at its left." 22)
(def ^:private list-pad-y "Points above and below a list's text." 2)

(def text-keys
  "What a level holds: its text as it is edited, and how far down it is
  scrolled, in pixels; a view of an inset takes them from the inset and
  gives them back."
  [:doc :undo :undo-tail :undo-chain :goal-x :upstream? :composition
   :insets :inset :next-inset-id :scroll])

(def ^:private view-keys
  "What a view of an inset has of its own besides `text-keys`, from the
  inset and where it is placed."
  [:layout :ctx :block-places :origin :view-h :view-w :clip])

(defn- mark-id [id] [:inset id])

(defn kind "Inset `i`'s kind." [i] (:kind i :section))
(defn section? [i] (= :section (kind i)))

(defn- across
  "Render pixels inset `i` takes across, beside its text."
  [app i]
  (if (section? i) (* 2 (+ (px app border) (px app pad-x))) (px app gutter)))

(defn folded? [i] (or (:collapsed? i) (nil? (:layout i))))

(defn- at
  "`level` with `f` of the inset at `path` down from it (or of itself, for
  an empty path)."
  [level path f]
  (if (seq path)
    (update-in level [:insets (first path)] at (rest path) f)
    (f level)))

(defn inset-at
  "The inset at `path` down from `app`, or nil."
  [app path]
  (when (seq path) (get-in app (vec (interleave (repeat :insets) path)))))

(defn innermost
  "The inset the caret is in, or over, at the end of the chain, or nil."
  [app]
  (loop [level app, found nil]
    (if-let [i (some->> (:inset level) (get (:insets level)))]
      (recur i i)
      found)))

(defn over
  "The folded inset the caret is over, or nil."
  [app]
  (let [i (innermost app)] (when (:collapsed? i) i)))

(defn path
  "The ids of the insets the caret is in, or over, down from the buffer's
  text, or nil."
  [app]
  (loop [level app, ids []]
    (if-let [id (:inset level)]
      (recur (get-in level [:insets id]) (conj ids id))
      (not-empty ids))))

(defn composing?
  "Whether the text with the caret, the buffer's or an inset's, is being
  composed."
  [app]
  (some? (:composition (or (innermost app) app))))

;; ---------------------------------------------------------------- checks

(defn- fit-checks
  "`checked` as one for each paragraph of text `t`, unticked past its end."
  [t checked]
  (vec (take (text/line-count t) (concat (map boolean checked) (repeat false)))))

(defn- reconcile
  "The ticks `flags` of text `old`'s paragraphs for those of `new`, an
  edit of it. Paragraphs the edit left alone keep theirs, as do changed
  ones that are, word for word, ones that were; of the rest, the first
  keeps the first's, as when a paragraph is split or joined, or all keep
  theirs when there are as many as there were."
  [old new flags]
  (let [flags (fit-checks old flags)
        [i ja jb] (text/changed-lines old new)
        olds (text/lines old i ja)
        news (text/lines new i jb)
        was  (subvec flags i ja)
        [mid used] (reduce (fn [[out used] s]
                             (if-let [j (first (keep-indexed (fn [j o] (when (and (not (used j)) (= o s)) j)) olds))]
                               [(conj out (was j)) (conj used j)]
                               [(conj out nil) used]))
                           [[] #{}] news)
        mid (vec (map-indexed (fn [k f]
                                (cond (some? f) f
                                      (= (count olds) (count news)) (was k)
                                      (and (zero? k) (seq olds) (not (used 0))) (was 0)
                                      :else false))
                              mid))]
    (vec (concat (subvec flags 0 i) mid (subvec flags ja)))))

(defn- sync-checks
  "Checklist `i` with its ticks those of its text as it is."
  [i]
  (let [t (get-in i [:doc :text])]
    (if (or (not= :checklist (:kind i)) (identical? t (:checked-text i)))
      i
      (assoc i :checked (reconcile (:checked-text i) t (:checked i)) :checked-text t))))

(defn- toggle-check*
  "Checklist `i` with the paragraph the caret is in ticked, or not."
  [i]
  (let [i (sync-checks i)
        k (first (text/line-at (text/of (get-in i [:doc :text])) (get-in i [:doc :caret])))]
    (update i :checked update k not)))

(defn toggle-check
  "Tick, or untick, the item the caret is in of the checklist at `path`."
  [app now path]
  (if (= :checklist (:kind (inset-at app path)))
    (touched (at app path toggle-check*) now)
    app))

;; ---------------------------------------------------------------- the document

(defn- paragraph-of
  "The paragraph of `level`'s text holding `pos`."
  [level pos]
  (first (text/line-at (text/of (get-in level [:doc :text])) pos)))

(defn- ordered
  "`level`'s insets, in order down its text, each with :after, the
  paragraph it is below (-1: above the first)."
  [level]
  (let [marks (get-in level [:doc :marks])]
    (->> (vals (:insets level))
         (keep (fn [i]
                 (when-let [pos (get marks (mark-id (:id i)))]
                   (assoc i :after (if (:above? i) -1 (paragraph-of level pos))))))
         (sort-by (juxt :after :order)))))

(defn snapshot
  "What `level`'s insets hold, as a file has it: [{:after k :text s :insets
  [...]}] in order down the text, each with its own insets, and its :kind,
  :title and :checked, if it has them."
  [level]
  (mapv (fn [i]
          (cond-> {:after (:after i) :text (str (get-in i [:doc :text])) :insets (snapshot i)}
            (:kind i)  (assoc :kind (:kind i))
            (:title i) (assoc :title (:title i))
            (= :checklist (:kind i)) (assoc :checked (:checked (sync-checks i)))))
        (ordered level)))

(defn- fresh
  "A new inset numbered `id`, of `spec`'s :text, :kind, :title and
  :checked, the caret at its start."
  [id order above? {:keys [text kind title checked]}]
  (let [doc (ed/doc (or text ""))]
    (cond-> {:id id :order order :above? above? :scroll 0 :doc doc :insets {} :next-inset-id 0}
      kind  (assoc :kind kind)
      title (assoc :title title)
      (= :checklist kind) (assoc :checked (fit-checks (:text doc) checked) :checked-text (:text doc)))))

(defn load-all
  "`level` with insets `specs`, as `snapshot` gives them, in place of any
  it had: :after past its last paragraph is below that."
  [level specs]
  (let [doc (reduce #(ed/unmark %1 (mark-id %2)) (:doc level) (keys (:insets level)))
        t   (text/of (:text doc))
        n   (text/line-count t)
        [doc insets next-id]
        (reduce (fn [[doc insets id] [order {:keys [after] :as spec}]]
                  (let [k      (min after (dec n))
                        above? (neg? k)
                        pos    (if above? 0 (+ (text/line-start t k) (count (text/line t k))))]
                    [(ed/mark doc (mark-id id) pos)
                     (assoc insets id (load-all (fresh id order above? spec) (:insets spec)))
                     (inc id)]))
                [doc {} (:next-inset-id level 0)]
                (map-indexed vector specs))]
    (-> level
        (assoc :doc doc :insets insets :next-inset-id next-id)
        (dissoc :inset))))

;; ---------------------------------------------------------------- the caret

(defn- collapse
  "`doc` with its selection, if any, given up, the caret where it is."
  [doc]
  (ed/move doc (:caret doc)))

(defn- leave-all
  "`level` with the caret out of the insets in it, their selections gone."
  [level]
  (if-let [id (:inset level)]
    (cond-> (dissoc level :inset)
      ;; an inset gone already has nothing to leave
      (get-in level [:insets id]) (update-in [:insets id] #(-> % leave-all (update :doc collapse))))
    level))

(defn leave
  "The caret out of the insets in `level`'s text, back in that text where
  it was."
  [level now]
  (if (:inset level) (touched (leave-all level) now) level))

(defn- enter
  "The caret into inset `id` of `level`, where it was there, or over it if
  it is folded: the level's selection goes."
  [level now id]
  (-> (leave level now)
      (update :doc collapse)
      (assoc :inset id)
      (touched now)))

;; ---------------------------------------------------------------- the view

(defn- intersect [[ax ay aw ah] [bx by bw bh]]
  (let [x0 (max ax bx), y0 (max ay by)
        x1 (min (+ ax aw) (+ bx bw)), y1 (min (+ ay ah) (+ by bh))]
    (when (and (< x0 x1) (< y0 y1)) [x0 y0 (- x1 x0) (- y1 y0)])))

(defn- clip
  "Where `level`'s text shows in the window, as [x y w h] render pixels, or
  nil if it doesn't."
  [level]
  (if (contains? level :clip)
    (:clip level)
    [0 (px level (:margin level)) (first (:size level)) (geo/view-height level)]))

(defn level-of
  "The app with inset `i`'s level in place of its text's, laid out and
  placed as `i` is, but not shown anywhere: see `view`."
  [app i]
  (-> (apply dissoc app text-keys)
      (merge (select-keys i text-keys))
      (assoc :layout (:layout i) :ctx (:layout-ctx i) :block-places (:block-places i []))))

(defn place-of
  "Inset `id` of `level` as it is placed, from its :block-places, or nil."
  [level id]
  (some #(when (= id (:inset %)) %) (:block-places level)))

(defn view
  "`level` as its inset `id` shows it: with the inset's level in place of
  its own, shown where the inset's text is (see hoatzin.app.geometry),
  within :clip, where that shows in the window (nil: nowhere); nil while
  it is folded."
  [level id]
  (when-let [[tx ty tw th] (:text (place-of level id))]
    (let [[ox oy] (geo/origin level)
          x (+ ox tx)
          y (+ oy (- ty (:scroll level)))]
      (assoc (level-of level (get-in level [:insets id]))
             :origin [x y] :view-h th :view-w tw
             :clip (some-> (clip level) (intersect [x y tw th]))))))

(defn- unview
  "`level` after view `v` of its inset `id` became `v`: the inset's level
  as `v` has it, everything else as `level` had it, but for what `v`
  changed."
  [level id v]
  (let [ks (concat text-keys view-keys)]
    (-> (apply dissoc v ks)
        (merge (select-keys level ks))
        (update-in [:insets id] #(merge (apply dissoc % text-keys) (select-keys v text-keys))))))

(defn in-view
  "`f` of the text with the caret: of a view of the inset it is in, at the
  end of the chain (see `view`), else of the app. Over a folded inset, of
  the text that holds that."
  [app f]
  (if-let [id (:inset app)]
    (if-let [v (view app id)]
      (unview app id (in-view v f))
      (f app))
    (f app)))

(defn list-kind? [i] (contains? #{:list :checklist} (kind i)))

(defn- holds?
  "Whether inset `i` can hold an inset of kind `k`: a section can hold
  any, a list only lists."
  [i k]
  (or (section? i) (and (list-kind? i) (contains? #{:list :checklist} k))))

(defn- in-holding
  "`f` of the innermost text with the caret that can hold an inset of
  kind `k`, and the kind of the inset whose text that is (nil for the
  buffer's): not one folded."
  ([level k f] (in-holding level k nil f))
  ([level k level-kind f]
   (if-let [id (:inset level)]
     (let [i (get-in level [:insets id])]
       (if-let [v (and (holds? i k) (view level id))]
         (unview level id (in-holding v k (kind i) f))
         (f level level-kind)))
     (f level level-kind))))

(defn innermost-view
  "The view of the inset the caret is in, at the end of the chain, else
  the app (or the text holding the inset it is over)."
  [app]
  (loop [level app]
    (if-let [v (some->> (:inset level) (view level))] (recur v) level)))

;; ---------------------------------------------------------------- adding and removing

(defn- add-here
  "`level`, the text of an inset of kind `level-kind` (nil for the
  buffer's), with a new inset of `spec` (see `fresh`), and the caret in
  it:
    - in a list, below the item the caret is in, after any others there;
    - if the caret is in an inset that can't hold it, or over a folded
      one, just after that, with a new line after it to go on writing in;
    - on an empty line, in that line's place, the line after it;
    - else below the line the caret is in, after any others there, with a
      new line after it."
  [level level-kind now spec]
  (let [id  (:next-inset-id level 0)
        old (:doc level)
        t   (text/of (:text old))
        cur (some->> (:inset level) (get (:insets level)))
        [k start line] (text/line-at t (:caret old))
        in-place? (and (not cur) (not (list-kind? {:kind level-kind})) (= "" line))
        ;; where its mark goes, and so the paragraph it is below
        pos (cond cur        (get-in old [:marks (mark-id (:id cur))])
                  in-place?  (max 0 (dec start))
                  :else      (+ start (count line)))
        above? (if cur (boolean (:above? cur)) (and in-place? (zero? k)))
        after  (if above? -1 (paragraph-of level pos))
        orders (map :order (filter #(= after (:after %)) (ordered level)))
        order  (if cur
                 (if-let [next (seq (filter #(> % (:order cur)) orders))]
                   (/ (+ (:order cur) (reduce min next)) 2.0)
                   (inc (:order cur)))
                 (inc (reduce max -1 orders)))
        new-line? (not (or in-place? (list-kind? {:kind level-kind})))
        ;; the new line goes after the paragraph, and so after the insets below it
        end (if above? 0 (let [[_ start line] (text/line-at t pos)] (+ start (count line))))]
    (-> (if new-line?
          (-> level
              (assoc :doc (-> old (ed/move end) (ed/insert "\n") (ed/move (:caret old))))
              (history/record (ed/move old end) end end "\n"))
          level)
        (update :doc ed/mark (mark-id id) pos)
        (assoc-in [:insets id] (fresh id order above? spec))
        (assoc :next-inset-id (inc id))
        (enter now id))))

(defn add
  "A new inset of `spec`, {:text s :kind k ...} (see `fresh`), or of
  string `spec`, in the innermost text the caret is in that can hold one,
  as `add-here` places it; the caret goes into it."
  [app now spec]
  (let [spec (if (string? spec) {:text spec} spec)]
    (in-holding app (kind spec) #(add-here %1 %2 now spec))))

(defn delete-line
  "`level` without the paragraph the caret is in, and its newline; the
  caret goes to the start of the one after it, else the one before. The
  insets below it stay where they are: below the paragraph before, or
  above the first. A lone paragraph is emptied. It is undone by itself."
  [level now]
  (let [old (:doc level)
        t   (text/of (:text old))
        n   (text/line-count t)
        [k start line] (text/line-at t (:caret old))
        end (+ start (count line))
        [lo hi] (cond (= n 1)   [0 end]
                      (zero? k) [0 (inc end)]
                      :else     [(dec start) end])
        below (when (and (zero? k) (> n 1)) (filter #(= 0 (:after %)) (ordered level)))
        doc (ed/delete old lo hi)
        caret (cond (= n 1) 0
                    (< k (dec n)) (if (zero? k) 0 start)
                    :else (text/line-start (text/of (:text doc)) (dec k)))]
    (-> (reduce (fn [level i]
                  (-> level
                      (assoc-in [:insets (:id i) :above?] true)
                      (assoc-in [:insets (:id i) :order] (+ 1000 (:order i)))))
                level below)
        (assoc :doc (ed/move doc caret) :goal-x nil :upstream? false)
        ;; a line deleted is undone on its own, not with the deleting before it
        (dissoc :undo-tail)
        (history/record old lo hi "")
        (touched now))))

(defn inset-after?
  "Whether position `pos` of `level`'s text ends a paragraph an inset is
  below: what comes after it is that inset."
  [level pos]
  (let [[k start line] (text/line-at (text/of (get-in level [:doc :text])) pos)]
    (and (= pos (+ start (count line)))
         (boolean (some #(= k (:after %)) (ordered level))))))

(defn remove-inset
  "The app without the inset at `path` (see `path`); if the caret was in
  it, it is in the text that held it."
  [app now path]
  (if (inset-at app path)
    (touched (at app (butlast path)
                 (fn [level]
                   (let [id (last path)]
                     (-> (if (= id (:inset level)) (leave-all level) level)
                         (update :doc ed/unmark (mark-id id))
                         (update :insets dissoc id)))))
             now)
    app))

(defn- toggle*
  "`level` with its section `id` folded to its header, or unfolded. Folding
  the one the caret is in leaves the caret over it; unfolding the one it
  is over puts it inside, where it was."
  [level id]
  (cond-> level
    (and (not (get-in level [:insets id :collapsed?])) (= id (:inset level)))
    (update-in [:insets id] #(-> % leave-all (update :doc collapse)))
    true (update-in [:insets id :collapsed?] not)))

(defn toggle
  "Fold `level`'s section `id`, or unfold it: see `toggle*`."
  [level now id]
  (touched (toggle* level id) now))

(defn toggle-at
  "Fold the section at `path`, or unfold it: see `toggle*`."
  [app now path]
  (if (section? (inset-at app path))
    (touched (at app (butlast path) #(toggle* % (last path))) now)
    app))

;; ---------------------------------------------------------------- renaming

(defn start-rename
  "Begin renaming the section at `path` in its header: see `rename`."
  [app path]
  (if (section? (inset-at app path))
    (-> (at app path #(assoc % :renaming (or (:title %) "")))
        (assoc :renaming path :dirty? true))
    app))

(defn renaming
  "The name being typed for the section being renamed, or nil."
  [app]
  (some->> (:renaming app) (inset-at app) :renaming))

(defn rename
  "The section being renamed with its name, as typed, `f` of it."
  [app f]
  (-> (at app (:renaming app) #(update % :renaming f)) (assoc :dirty? true)))

(defn end-rename
  "Stop renaming: with `keep?`, the section takes the name typed, or with
  none its title goes."
  [app keep?]
  (-> (at app (:renaming app)
          (fn [i]
            (let [s (str/trim (:renaming i))
                  i (dissoc i :renaming)]
              (cond (not keep?) i
                    (seq s)     (assoc i :title s)
                    :else       (dissoc i :title)))))
      (dissoc :renaming)
      (assoc :dirty? true)))

;; ---------------------------------------------------------------- lists

(defn- bounds
  "Where paragraph `k` of text `t` starts and ends."
  [t k]
  (let [s (text/line-start t k)] [s (+ s (count (text/line t k)))]))

(defn- restructured
  "List `l` after its text was changed outside its history: the history
  goes, as it no longer fits, and its ticks are of its text as it is."
  [l]
  (cond-> (dissoc l :undo :undo-tail :undo-chain)
    (:checked l) (assoc :checked-text (get-in l [:doc :text]))))

(defn- attach
  "List `l` with insets `children`, in order, below its item `k`."
  [l k children]
  (let [[_ end] (bounds (text/of (get-in l [:doc :text])) k)]
    (reduce (fn [l c]
              (let [id (:next-inset-id l 0)]
                (-> l
                    (update :doc ed/mark (mark-id id) end)
                    (assoc-in [:insets id] (-> (leave-all c) (assoc :id id :order (inc (reduce max -1 (map :order (vals (:insets l))))) :above? false)))
                    (assoc :next-inset-id (inc id)))))
            l children)))

(defn- take-item
  "[`l` without its item `k`, the item]: the item {:text :checked
  :children}, its children the insets below it, which go with it. The
  last item taken leaves one empty."
  [l k]
  (let [l    (sync-checks l)
        t    (text/of (get-in l [:doc :text]))
        n    (text/line-count t)
        kids (filter #(= k (:after %)) (ordered l))
        l    (reduce (fn [l c] (-> l (update :doc ed/unmark (mark-id (:id c))) (update :insets dissoc (:id c))))
                     l kids)
        [s0 e0] (bounds t k)
        [lo hi] (cond (= n 1)    [0 (count t)]
                      (zero? k)  [s0 (inc e0)]
                      :else      [(dec s0) e0])
        checked (:checked l)]
    [(-> l
         (update :doc #(ed/move (ed/delete % lo hi) 0))
         (cond-> checked (assoc :checked (if (= n 1) [false] (vec (concat (subvec checked 0 k) (subvec checked (inc k)))))))
         restructured)
     {:text (text/line t k) :checked (boolean (get checked k))
      :children (map #(dissoc % :after) kids)}]))

(defn- put-item
  "List `l` with `item`, as `take-item` gives it, as its item `k`."
  [l k {:keys [text checked children]}]
  (let [l (sync-checks l)
        t (text/of (get-in l [:doc :text]))
        n (text/line-count t)
        [pos s] (if (= k n) [(second (bounds t (dec n))) (str "\n" text)] [(text/line-start t k) (str text "\n")])]
    (-> l
        (update :doc #(-> % (ed/move pos) (ed/insert s)))
        (cond-> (:checked l) (update :checked #(vec (concat (subvec % 0 k) [checked] (subvec % k)))))
        restructured
        (attach k children))))

(defn- new-list
  "A list of kind `k` numbered `id` of `items`, as `take-item` gives them."
  [id order above? k items]
  (let [l (fresh id order above? {:kind k :text (str/join "\n" (map :text items)) :checked (map :checked items)})]
    (reduce (fn [l [j item]] (attach l j (:children item))) l (map-indexed vector items))))

(defn- add-list
  "`level` with a list of kind `k` of `items` below its paragraph `after`,
  after any other insets there; and its id."
  [level after k items]
  (let [id (:next-inset-id level 0)
        t (text/of (get-in level [:doc :text]))
        above? (neg? after)
        pos (if above? 0 (second (bounds t after)))
        order (inc (reduce max -1 (map :order (filter #(= after (:after %)) (ordered level)))))]
    [(-> level
         (update :doc ed/mark (mark-id id) pos)
         (assoc-in [:insets id] (new-list id order above? k items))
         (assoc :next-inset-id (inc id)))
     id]))

(defn- caret-to
  "The app with the caret in the text at `path` (the buffer's, for an
  empty one), at `pos`."
  [app path pos]
  (let [app (leave-all app)
        app (reduce (fn [app n] (at app (subvec path 0 n) #(assoc % :inset (path n)))) app (range (count path)))]
    (at app path #(update % :doc ed/move pos))))

(defn- item-of
  "The item the caret is in of the list at the end of the chain: [path k
  col], or nil if it isn't in a list."
  [app]
  (let [l (innermost app)]
    (when (and (list-kind? l) (not (:collapsed? l)))
      (let [{:keys [text caret]} (:doc l)
            [k start] (text/line-at (text/of text) caret)]
        [(path app) k (- caret start)]))))

(defn- indent
  "The app with item `k` of the list at `path` in a sublist below the item
  before it, at its end, the caret `col` along it; nil for the first item."
  [app path k col]
  (when (pos? k)
    (let [l (inset-at app path)
          [l item] (take-item l k)
          target (last (filter #(and (list-kind? %) (= (dec k) (:after %))) (ordered l)))
          [l sub k2] (if target
                       (let [k2 (text/line-count (text/of (get-in target [:doc :text])))]
                         [(update-in l [:insets (:id target)] put-item k2 item) (:id target) k2])
                       (let [[l id] (add-list l (dec k) (kind l) [item])] [l id 0]))
          sub-path (conj path sub)
          app (at app path (constantly l))
          t (text/of (get-in (inset-at app sub-path) [:doc :text]))]
      (caret-to app sub-path (+ (text/line-start t k2) (min col (count (:text item))))))))

(defn- dedent
  "The app with item `k` of the list at `path` out in the list holding it,
  just after it, the items after it going with it as its sublist, the
  caret `col` along it; nil if it isn't held by a list."
  [app path k col]
  (let [outer (pop path)
        id    (peek path)]
    (when (and (seq outer) (list-kind? (inset-at app outer)))
      (let [p   (inset-at app outer)
            a   (:after (some #(when (= id (:id %)) %) (ordered p)))
            l   (inset-at app path)
            n   (text/line-count (text/of (get-in l [:doc :text])))
            ;; the items after it, from the last, so each is where it was
            [l later] (reduce (fn [[l later] j] (let [[l item] (take-item l j)] [l (cons item later)]))
                              [l ()] (range (dec n) k -1))
            [l item] (take-item l k)
            ;; a list left with nothing goes, unless sublists are above its first item
            p (if (and (zero? k) (empty? (:insets l)))
                (-> p (update :doc ed/unmark (mark-id id)) (update :insets dissoc id) (dissoc :inset))
                (assoc-in p [:insets id] l))
            p (put-item p (inc a) item)
            p (if (seq later) (first (add-list p (inc a) (kind l) later)) p)
            app (at app outer (constantly p))
            t (text/of (get-in (inset-at app outer) [:doc :text]))]
        (caret-to app outer (+ (text/line-start t (inc a)) (min col (count (:text item)))))))))

(defn- level-at
  "The level at `path`: the buffer's text-keys for an empty one."
  [app path]
  (if (seq path) (inset-at app path) (select-keys app text-keys)))

(defn- restore
  "The app with the level at `path` as `saved`."
  [app path saved]
  (if (seq path) (at app path (constantly saved)) (merge app saved)))

(defn cycle-indent
  "Tab on a list item: the first indents it into a sublist of the item
  before it, the next takes it out to the list holding its list, if any,
  and the next puts it back where it began; and so on, while nothing else
  happens between. nil if the caret isn't in a list."
  [app now]
  (when-let [[here k col] (item-of app)]
    (let [l    (innermost app)
          tc   (:list-cycle app)
          same (and tc (= here (:at tc))
                    (identical? (get-in l [:doc :text]) (:text tc))
                    (= (get-in l [:doc :caret]) (:caret tc)))
          tc   (if same
                 tc
                 {:rel 0 :path here :k k :col col :outer (pop here) :saved (level-at app (pop here))})
          {:keys [rel outer saved]} tc
          start (restore app outer saved)
          in    #(indent start (:path tc) (:k tc) (:col tc))
          out   #(dedent start (:path tc) (:k tc) (:col tc))
          [app rel] (case rel
                      0  (if-let [a (in)] [a 1] (if-let [a (out)] [a -1] [app 0]))
                      1  (if-let [a (out)] [a -1] [start 0])
                      -1 [start 0])
          l (innermost app)]
      (-> app
          (assoc :list-cycle (assoc tc :rel rel :at (path app)
                                    :text (get-in l [:doc :text]) :caret (get-in l [:doc :caret])))
          (touched now)))))

(defn list-return
  "Return in a list: on its last item, if that is empty, the item goes
  and the caret leaves the list (`all?`: every list it is in, on any
  item) for a new line just after it, in the text that held it. nil
  otherwise, or if the caret isn't in a list."
  [app now all?]
  (when-let [[path k] (item-of app)]
    (let [l (inset-at app path)
          t (text/of (get-in l [:doc :text]))
          n (text/line-count t)
          empty-last? (and (= k (dec n)) (= "" (text/line t k)))]
      (when (or all? empty-last?)
        (let [;; the lists left, from the outermost
              lists (if all?
                      (count (take-while #(list-kind? (inset-at app (subvec path 0 %))) (range (count path) 0 -1)))
                      1)
              outer (subvec path 0 (- (count path) lists))
              top   (subvec path 0 (inc (count outer)))
              a     (:after (some #(when (= (peek top) (:id %)) %) (ordered (level-at app outer))))
              app   (cond (not empty-last?) app
                          (= 1 n) (remove-inset app now path)
                          :else (at app path #(first (take-item % k))))
              app   (at app outer
                        (fn [o]
                          (let [old (:doc o)
                                t (text/of (:text old))
                                pos (if (neg? a) 0 (second (bounds t a)))]
                            (-> o
                                (assoc :doc (-> old (ed/move pos) (ed/insert "\n")))
                                (history/record (ed/move old pos) pos pos "\n")))))
              t     (text/of (get-in (level-at app outer) [:doc :text]))]
          (touched (caret-to app outer (if (neg? a) 0 (text/line-start t (inc a)))) now))))))

(defn- tick-at
  "`level` with the item of its checklist `id` at render y `y` ticked or
  not, if `x` is in the checklist's gutter; else nil."
  [level now id x y]
  (let [i (get-in level [:insets id])]
    (when (= :checklist (:kind i))
      (when-let [v (view level id)]
        (when (< x (first (:origin v)))
          (let [L (:layout v)
                [k] (geo/point->line v x y)
                j (first (text/line-at (text/of (:text L)) (layout/line-start L k)))]
            (-> level
                (update-in [:insets id] #(update (sync-checks %) :checked update j not))
                (touched now))))))))

;; ---------------------------------------------------------------- layout

(defn sync-layouts
  "Lay out again the text of each unfolded inset, all the way down, that
  has changed since it last was, or every one, if its context has; one
  context to each width, made as it is wanted. Checklists' ticks follow
  their text."
  [app]
  (let [wrap (get-in app [:ctx :width])]
    (if (or (nil? wrap) (empty? (:insets app)))
      app
      (let [ctxs (volatile! (:inset-ctxs app {}))
            changed? (volatile! false)
            ctx-of (fn [width]
                     (or (@ctxs width)
                         (let [c (layout/context (:font app) width)]
                           (vswap! ctxs assoc width c)
                           c)))
            sync (fn sync [level width]
                   (update level :insets update-vals
                           (fn [i]
                             (let [i (sync-checks i)]
                               (if (:collapsed? i)
                                 i
                                 (let [w   (max 1 (- width (across app i)))
                                       ctx (ctx-of w)
                                       i (if (and (identical? ctx (:layout-ctx i))
                                                  (display/same-display? (display/display-key i) (:laid-out i)))
                                           i
                                           (do (vreset! changed? true)
                                               (assoc i :layout (layout/layout ctx (display/display-text i))
                                                      :laid-out (display/display-key i) :layout-ctx ctx)))]
                                   (sync i w)))))))
            app (sync app wrap)]
        (cond-> (assoc app :inset-ctxs @ctxs) @changed? (assoc :dirty? true))))))

(defn release-contexts!
  "Release the insets' layout contexts."
  [app]
  (run! layout/release-context (vals (:inset-ctxs app))))

(defn- content-height
  "How tall inset `i`'s text is, with its insets, in pixels."
  [i]
  (+ (* (layout/line-height (:layout i)) (layout/line-count (:layout i)))
     (reduce + 0 (map :height (:block-places i)))))

(defn- shown-height
  "How much of inset `i`'s text it shows, in pixels: a section no more
  than `max-rows` lines."
  [i]
  (cond-> (content-height i)
    (section? i) (min (* max-rows (layout/line-height (:layout i))))))

(defn- most-scroll [i] (- (content-height i) (shown-height i)))

(defn placed
  "Inset `i` with its insets placed, as `l`, its level, has them: and its
  scroll within its text."
  [i l]
  (let [i (assoc i :insets (:insets l) :block-places (:block-places l))]
    (assoc i :scroll (min (:scroll i 0) (most-scroll i)))))

(defn- dim [app] (mapv #(quot (+ (* 2 %1) %2) 3) (:foreground app) (:window-background app)))

(defn- section-node
  "Section `i` of `level` as a box (see hoatzin.lib.ui): its header,
  saying its title or `title`, highlighted while the caret is over it, and
  unless it is folded, room for as much of its text, with its own insets
  placed in it, as it shows."
  [level i title]
  (let [folded? (folded? i)
        over?   (and (:collapsed? i) (= (:id i) (:inset level)))
        first-line (first (text/lines (text/of (get-in i [:doc :text])) 0 1))
        n (when (:layout i) (layout/line-count (:layout i)))
        name (cond (:renaming i) (:renaming i)
                   (:title i) (:title i)
                   (and folded? (seq first-line)) (str title ": " first-line)
                   :else title)]
    {:kind :box
     :style {:border border :border-color (if over? (:ui-focus level) (:ui-border level))
             :background (:background level)}
     :children
     (cond-> [{:kind :box :inset-header (:id i)
               :style {:direction :row :gap 8 :align :center
                       :padding [header-pad (- pad-x border)]
                       :background (if over? (:ui-highlight level) (:window-background level))}
               :children (cond-> [{:kind :label :text (if folded? "▸" "▾") :style {:color (dim level)}}
                                  {:kind :label :inset-title (:id i) :text name
                                   :style {:width 0 :grow 1 :color (:foreground level)}}]
                           (and folded? n) (conj {:kind :label :text (str n (if (= 1 n) " line" " lines"))
                                                  :style {:color (dim level)}}))}]
       (not folded?)
       (conj {:kind :box :inset-body (:id i)
              :style {:padding [pad-y pad-x]
                      :height (+ (/ (shown-height i) (:density level)) (* 2 pad-y))}}))}))

(defn- list-node
  "List or checklist `i` as a box: its text, with room at its left for its
  bullets or boxes, which are drawn with it."
  [level i]
  {:kind :box
   :children [{:kind :box :inset-body (:id i)
               :style {:padding [list-pad-y 0 list-pad-y gutter]
                       :height (+ (/ (shown-height i) (:density level)) (* 2 list-pad-y))}}]})

(defn blocks
  "`level`'s insets as blocks to place (see hoatzin.app.boxes/place-blocks):
  {:id :inset :node :pos :line :order}, :line the visual line it is below
  (-1: above the first), a section's header saying `title` unless it has
  its own."
  [level title]
  (let [L (:layout level)
        marks (get-in level [:doc :marks])]
    (keep (fn [i]
            (when-let [pos (get marks (mark-id (:id i)))]
              (when (or (:layout i) (:collapsed? i))
                (let [pos (display/shown-pos level pos)]
                  {:id (mark-id (:id i)) :inset (:id i) :pos pos :order (:order i)
                   :node (if (section? i) (section-node level i title) (list-node level i))
                   :line (if (:above? i) (dec (layout/first-line L pos)) (layout/last-line L pos))}))))
          (vals (:insets level)))))

(defn parts
  "Where a placed inset's header and text are, from its placed boxes
  `placed`: {:header rect :title rect :body rect :text rect}, :body the
  box the text is in and :text inside its padding; no :body while it is
  folded, and no header for a list."
  [placed]
  (let [find (fn [k] (some #(when (get-in % [:node k]) %) placed))]
    (merge {:header (:rect (find :inset-header)) :title (:content (find :inset-title))}
           (when-let [b (find :inset-body)] {:body (:rect b) :text (:content b)}))))

(defn title-rect
  "Where the title of the section at `path` is, in render pixels, or nil
  if it isn't shown."
  [app path]
  (loop [level app, [id & more] path]
    (if more
      (when-let [v (view level id)] (recur v more))
      (when-let [[x y w h] (:title (place-of level id))]
        (let [[ox oy] (geo/origin level)]
          [(+ ox x) (+ oy (- y (:scroll level))) w h])))))

(defn caret-top
  "The top of the caret's line, in `level`'s content pixels: in the inset
  it is in, if any, as placed there, or the top of the one it is over."
  [level]
  (if-let [id (:inset level)]
    (if-let [v (view level id)]
      (let [[_ ty] (:text (place-of level id))]
        (+ ty (- (caret-top v) (:scroll v))))
      (or (:top (place-of level id)) 0))
    (geo/line-top level (second (geo/caret-place level)))))

(defn follow
  "Scroll the insets the caret is in, from the innermost out, just enough
  to show its line."
  [level]
  (if-let [v (some->> (:inset level) (view level))]
    (let [v  (follow v)
          lh (layout/line-height (:layout v))
          top (caret-top v)
          s  (:scroll v)
          vh (geo/view-height v)]
      (unview level (:inset level)
              (assoc v :scroll (cond (< top s) top
                                     (> (+ top lh) (+ s vh)) (- (+ top lh) vh)
                                     :else s))))
    level))

;; ---------------------------------------------------------------- crossing

;; Up and down move a visual line at a time, whatever text it is in. A
;; stop is where the caret can be: [path k], visual line k of the text at
;; `path` (the buffer's for []), or [path :header], over the folded inset
;; at `path`. From the caret, the next stop down is the first inset below
;; its line, else the next line, else, at the end of its text, the next
;; after the inset holding that text, and so on out; up is the same the
;; other way.

(defn- inset-blocks
  "`level`'s insets as placed below visual line `k` (-1: above the
  first), in order."
  [level k]
  (filter #(and (:inset %) (= k (:line %))) (:block-places level)))

(declare first-stop last-stop)

(defn- stop-into
  "The first (or `last?` last) stop in `level`'s inset `b`, as placed: in
  its text, or over its header if it is folded."
  [level pre b last?]
  (let [q (conj pre (:inset b))]
    (if-let [v (view level (:inset b))]
      ((if last? last-stop first-stop) v q)
      [q :header])))

(defn- first-stop [level pre]
  (if-let [b (first (inset-blocks level -1))]
    (stop-into level pre b false)
    [pre 0]))

(defn- last-stop [level pre]
  (let [k (dec (layout/line-count (:layout level)))]
    (if-let [b (last (inset-blocks level k))]
      (stop-into level pre b true)
      [pre k])))

(defn- next-in
  "The next stop down in `level`, at `pre`, after its line `k`, or after its
  inset `from` below that line; nil at the end of its text."
  [level pre k from]
  (let [bs (inset-blocks level k)
        bs (if from (rest (drop-while #(not= from (:inset %)) bs)) bs)]
    (cond (seq bs) (stop-into level pre (first bs) false)
          (< (inc k) (layout/line-count (:layout level))) [pre (inc k)])))

(defn- prev-in
  "The next stop up in `level`, at `pre`, before its line `k`, or before
  its inset `from` below line `k`; nil at the start of its text."
  [level pre k from]
  (if from
    (if-let [b (last (take-while #(not= from (:inset %)) (inset-blocks level k)))]
      (stop-into level pre b true)
      (when (>= k 0) [pre k]))
    (if-let [b (last (inset-blocks level (dec k)))]
      (stop-into level pre b true)
      (when (pos? k) [pre (dec k)]))))

(defn- chain
  "The texts from the buffer's to the one the caret is in (or that holds
  the folded inset it is over), each [level path], as views."
  [app]
  (loop [level app, pre [], out [[app []]]]
    (if-let [v (some->> (:inset level) (view level))]
      (let [pre (conj pre (:inset level))] (recur v pre (conj out [v pre])))
      out)))

(defn- next-stop
  "The stop the caret goes to down (or up), from `chain`, its texts, the
  caret on line `k` of the innermost, or over its inset `from`; nil if
  there is none."
  [chain k from down?]
  (let [[level pre] (peek chain)]
    (or ((if down? next-in prev-in) level pre k from)
        (when (> (count chain) 1)
          (let [outer (pop chain)
                id    (peek pre)]
            (next-stop outer (:line (place-of (first (peek outer)) id)) id down?))))))

(defn- chain-to
  "The app with the caret in the text at `path`, or over the folded inset
  at its end, the chain of :inset ids down to it and no further."
  [app path]
  (reduce (fn [app n] (at app (subvec path 0 n) #(assoc % :inset (path n))))
          (leave-all app)
          (range (count path))))

(defn- go-stop
  "The caret to `stop`, nearest render x `x`: on its line, or over its
  header."
  [app now [q k] x]
  (let [app (chain-to app q)]
    (if (= :header k)
      (let [holder (innermost-view app)]
        (-> (at app (pop q) #(assoc % :goal-x (- x (first (geo/origin holder)))))
            (touched now)))
      (in-view app (fn [v]
                     (let [x (- x (first (geo/origin v)))]
                       (move-on-line v now false k (layout/position-at (:layout v) k x) x)))))))

(defn cross
  "Up or down `key`, without modifiers: the caret to the visual line above
  or below, whatever text it is in, or over a folded inset's header, at
  any depth; nil if that is the next line of the text it is in already,
  as the keys move it there themselves, or there is none."
  [app now key mod]
  (when (and (zero? (bit-and mod (bit-or sdl/KMOD-SHIFT sdl/KMOD-GUI sdl/KMOD-CTRL)))
             (or (= key sdl/K-UP) (= key sdl/K-DOWN)))
    (let [down?   (= key sdl/K-DOWN)
          chain   (chain app)
          [level pre] (peek chain)
          over-id (when (over app) (:inset level))
          [cx k]  (if over-id
                    [(first (geo/caret-place level)) (:line (place-of level over-id))]
                    (geo/caret-place level))
          x       (+ (first (geo/origin level)) (or (:goal-x level) cx))
          [q k2 :as stop] (next-stop chain k over-id down?)]
      (when (and stop (not (and (nil? over-id) (= q pre) (number? k2))))
        (go-stop app now stop x)))))

;; ---------------------------------------------------------------- the pointer

(defn- inside? [[rx ry rw rh] x y] (and rx (<= rx x) (< x (+ rx rw)) (<= ry y) (< y (+ ry rh))))

(defn hit
  "`level`'s inset under render pixel (x, y), where its text shows, as
  {:id :part}, :part :header or :body; or nil."
  [level x y]
  (let [[ox oy] (geo/origin level)
        cx (- x ox)
        cy (+ (- y oy) (:scroll level))]
    (when (and (>= y oy) (< y (+ oy (geo/view-height level))))
      (some (fn [b]
              (when (:inset b)
                (cond (inside? (:header b) cx cy) {:id (:inset b) :part :header}
                      (inside? (:body b) cx cy)   {:id (:inset b) :part :body})))
            (:block-places level)))))

(defn click
  "A click at render pixel (x, y), if it is on one of `level`'s insets,
  at any depth: on a section's header, fold or unfold it; on a
  checklist's box, tick it or not; in its text, the caret there, by `on-click` of the view, which takes it and gives it
  back. nil if the click is on none."
  [level now x y on-click]
  (when-let [{:keys [id part]} (hit level x y)]
    (if (= :header part)
      (toggle level now id)
      (or (tick-at level now id x y)
          (let [level (if (= id (:inset level)) level (enter level now id))
                v (view level id)]
            (unview level id (or (click v now x y on-click) (on-click (leave v now)))))))))

(defn on-wheel
  "The wheel over render pixel (x, y): the innermost section there that
  has more than it shows scrolls; else nil."
  [level x y dy]
  (when-let [{:keys [id part]} (and x (hit level x y))]
    (when (= :body part)
      (let [v (view level id)]
        (if-let [v' (on-wheel v x y dy)]
          (unview level id v')
          (let [i (get-in level [:insets id])
                most (most-scroll i)]
            (when (pos? most)
              (let [s (- (:scroll i 0) (* dy (:wheel-lines level) (layout/line-height (:layout i))))]
                (-> level
                    (assoc-in [:insets id :scroll] (long (max 0 (min most (Math/round (double s))))))
                    (assoc :dirty? true))))))))))

(defn thumb
  "The scroll thumb of inset `i` placed as `b`, in its level's content
  pixels, or nil if it shows all it holds: [x y w h], at the right of its
  text."
  [app i b]
  (when-let [[bx _ bw] (:body b)]
    (let [most (most-scroll i)]
      (when (pos? most)
        (let [[_ ty _ th] (:text b)
              h (max (px app 12) (quot (* th th) (content-height i)))
              w (px app thumb-width)]
          [(- (+ bx bw) (quot (+ (px app pad-x) w) 2))
           (+ ty (long (Math/round (/ (* (- th h) (double (:scroll i 0))) most))))
           w h])))))

(defn markers
  "What a list or checklist shows before its items in view: for each
  visual line [k0, k1) of view `v` of it that begins an item, [k checked?]
  (checked? nil for a list)."
  [v i k0 k1]
  (let [L (:layout v)
        t (text/of (:text L))
        checked (:checked (sync-checks i))]
    (for [k (range k0 k1)
          :let [pos (layout/line-start L k)]
          :when (= k (layout/first-line L pos))]
      [k (when (= :checklist (:kind i)) (boolean (get checked (first (text/line-at t pos)))))])))
