(ns hoatzin.tree
  "A persistent B+-tree of items with summed measures: the shape under both
  the document's text (items are lines) and its layout (items are typeset
  paragraphs).

  Every node caches three sums over its items: :n, the item count; :len, a
  length; and :w, a weight. What :len and :w measure is up to the tree's
  user, given as a `spec` {:len f :w f} of item -> long. Indexing by any of
  the three, splicing a run of items and folding over a range are all
  O(log n) plus the items touched, and an edit shares every subtree it
  doesn't touch with the tree it came from.

  Unchanged items stay the same objects, so two trees that share history
  are compared by identity: `common-prefix`/`common-suffix` skip identical
  subtrees whole.")

(def ^:private max-kids 32)
(def ^:private min-kids 8)

(deftype Node [^long n ^long len ^long w kids leaf?])

(defn n ^long [^Node t] (.-n t))
(defn len ^long [^Node t] (.-len t))
(defn w ^long [^Node t] (.-w t))

;; ---------------------------------------------------------------- building

(defn- leaf [spec items]
  (let [lf (:len spec), wf (:w spec), c (count items)]
    (loop [k 0, l 0, ww 0]
      (if (= k c)
        (Node. c l ww items true)
        (let [it (nth items k)]
          (recur (inc k) (+ l (long (lf it))) (+ ww (long (wf it)))))))))

(defn- branch [kids]
  (let [c (count kids)]
    (loop [k 0, nn 0, l 0, ww 0]
      (if (= k c)
        (Node. nn l ww kids false)
        (let [^Node kid (nth kids k)]
          (recur (inc k) (+ nn (.-n kid)) (+ l (.-len kid)) (+ ww (.-w kid))))))))

(defn- chunks
  "`v` cut into the fewest runs of at most `max-kids`, as even as possible."
  [v]
  (let [c (count v)]
    (if (<= c max-kids)
      [v]
      (let [parts (quot (+ c max-kids -1) max-kids)]
        (mapv (fn [p] (into [] (subvec v (quot (* p c) parts) (quot (* (inc p) c) parts))))
              (range parts))))))

(defn- leaves [spec items] (mapv #(leaf spec %) (chunks items)))
(defn- branches [nodes] (mapv branch (chunks nodes)))

(defn- root
  "One node over `nodes`, all of one height: grown a level at a time while
  there are several, and stripped of single-child levels."
  [spec nodes]
  (loop [nodes nodes]
    (case (count nodes)
      0 (leaf spec [])
      1 (loop [^Node t (first nodes)]
          (if (and (not (.-leaf? t)) (= 1 (count (.-kids t))))
            (recur (first (.-kids t)))
            t))
      (recur (branches nodes)))))

(defn build
  "A tree over the vector `items`."
  [spec items]
  (root spec (leaves spec (vec items))))

;; ---------------------------------------------------------------- reading

(defn- measure ^long [^Node t dim]
  (case dim :n (.-n t) :len (.-len t) :w (.-w t)))

(defn- item-measure ^long [spec dim it]
  (case dim :n 1 :len (long ((:len spec) it)) :w (long ((:w spec) it))))

(defn locate
  "The item where the running sum of `dim` (:n, :len or :w) passes `x`: the
  first whose span [before, before + its measure) holds x, or the last item
  when x is past them all. Returns [item index len-before w-before]."
  [spec ^Node t dim x]
  (let [x (long x)]
    (loop [^Node t t, x x, i 0, l 0, ww 0]
      (let [kids (.-kids t)
            last-k (dec (count kids))]
        (if (.-leaf? t)
          (loop [k 0, x x, l l, ww ww]
            (let [it (nth kids k)
                  m (item-measure spec dim it)]
              (if (or (< x m) (= k last-k))
                [it (+ i k) l ww]
                (recur (inc k) (- x m)
                       (+ l (long ((:len spec) it))) (+ ww (long ((:w spec) it)))))))
          (let [[kid x i l ww]
                (loop [k 0, x x, i i, l l, ww ww]
                  (let [^Node kid (nth kids k)
                        m (measure kid dim)]
                    (if (or (< x m) (= k last-k))
                      [kid x i l ww]
                      (recur (inc k) (- x m) (+ i (.-n kid)) (+ l (.-len kid)) (+ ww (.-w kid))))))]
            (recur kid x i l ww)))))))

(defn get-item [spec t i] (first (locate spec t :n i)))

(defn fold
  "Reduce `f` over the items [i, j) in order, as (f acc item index
  len-before w-before). Honours `reduced`."
  [spec ^Node t i j f init]
  (let [lf (:len spec), wf (:w spec)
        step (fn step [acc ^Node t off l ww]
               ;; t's items start at index `off`, after `l` and `ww`
               (let [kids (.-kids t), c (count kids)]
                 (if (.-leaf? t)
                   (loop [k 0, acc acc, l l, ww ww]
                     (if (or (= k c) (>= (+ off k) j) (reduced? acc))
                       acc
                       (let [it (nth kids k)]
                         (recur (inc k)
                                (if (>= (+ off k) i) (f acc it (+ off k) l ww) acc)
                                (+ l (long (lf it))) (+ ww (long (wf it)))))))
                   (loop [k 0, acc acc, off off, l l, ww ww]
                     (if (or (= k c) (>= off j) (reduced? acc))
                       acc
                       (let [^Node kid (nth kids k)
                             nxt (+ off (.-n kid))]
                         (recur (inc k)
                                (if (> nxt i) (step acc kid off l ww) acc)
                                nxt (+ l (.-len kid)) (+ ww (.-w kid)))))))))]
    (unreduced (step init t 0 0 0))))

(defn items
  "The items [i, j) as a vector."
  ([spec t] (items spec t 0 (n t)))
  ([spec t i j] (fold spec t i j (fn [acc it _ _ _] (conj acc it)) [])))

;; ---------------------------------------------------------------- splicing

(defn- size [^Node t] (count (.-kids t)))

(defn- rebalance
  "Merge undersized nodes in kids [a, b) into their neighbours: re-chunk the
  window, one either side included, from the kids of the nodes in it."
  [spec kids a b]
  (let [a (max 0 (dec a))
        b (min (count kids) (inc b))
        window (subvec kids a b)]
    (if (or (< (count window) 2) (not-any? #(< (size %) min-kids) window))
      kids
      (let [^Node t0 (first window)
            grand (into [] (mapcat #(.-kids ^Node %)) window)
            merged (if (.-leaf? t0) (leaves spec grand) (branches grand))]
        (-> (subvec kids 0 a) (into merged) (into (subvec kids b)))))))

(defn- child-at
  "The kid holding item index `i` of `kids`, with `end?` the kid whose
  items end at or after `i` (for a range's exclusive end): [k offset]."
  [kids i end?]
  (let [last-k (dec (count kids))]
    (loop [k 0, off 0]
      (let [nxt (+ off (n (nth kids k)))]
        (if (or (= k last-k) (if end? (<= i nxt) (< i nxt)))
          [k off]
          (recur (inc k) nxt))))))

(defn- splice*
  "The nodes, all of `t`'s height, holding t's items with [i, j) replaced
  by `new`. There may be none, or several."
  [spec ^Node t i j new]
  (let [kids (.-kids t)]
    (if (.-leaf? t)
      (let [items (-> (subvec kids 0 i) (into new) (into (subvec kids j)))]
        (if (empty? items) [] (leaves spec items)))
      (let [[a oa] (child-at kids i false)
            [b ob] (if (= i j) [a oa] (child-at kids j true))
            mid (if (= a b)
                  (splice* spec (nth kids a) (- i oa) (- j oa) new)
                  (into (splice* spec (nth kids a) (- i oa) (n (nth kids a)) new)
                        (splice* spec (nth kids b) 0 (- j ob) [])))
            kids (rebalance spec
                            (-> (subvec kids 0 a) (into mid) (into (subvec kids (inc b))))
                            a (+ a (count mid)))]
        (if (empty? kids) [] (branches kids))))))

(defn splice
  "The tree with items [i, j) replaced by the vector `new`."
  [spec t i j new]
  (root spec (splice* spec t i j (vec new))))

;; ---------------------------------------------------------------- comparing

(defn- aligned
  "How many of the first (or with `back?`, last) items of `a` and `b` are
  identical, when both are at the same height and start (end) together.
  Returns [count status]: :differ at a difference, :same when they are
  identical throughout, :lost when one ran out first, so that the rest is
  no longer aligned."
  [^Node a ^Node b back?]
  (cond
    (identical? a b) [(.-n a) :same]
    (not= (.-leaf? a) (.-leaf? b)) [0 :lost]
    :else
    (let [ka (.-kids a), kb (.-kids b)
          ca (count ka), cb (count kb)
          c (min ca cb)
          at (fn [v cnt k] (nth v (if back? (- cnt 1 k) k)))]
      (if (.-leaf? a)
        (loop [k 0]
          (cond (= k c) [k (if (= ca cb) :same :lost)]
                (identical? (at ka ca k) (at kb cb k)) (recur (inc k))
                :else [k :differ]))
        (loop [k 0, acc 0]
          (if (= k c)
            [acc (if (= ca cb) :same :lost)]
            (let [[m status] (aligned (at ka ca k) (at kb cb k) back?)
                  acc (+ acc m)]
              (if (= status :same) (recur (inc k) acc) [acc status]))))))))

(defn common-prefix
  "How many leading items `a` and `b` share, by identity."
  [spec a b]
  (let [[m status] (aligned a b false)
        c (min (n a) (n b))]
    (if (= status :lost)
      (loop [k m]
        (if (and (< k c) (identical? (get-item spec a k) (get-item spec b k))) (recur (inc k)) k))
      (min m c))))

(defn common-suffix
  "How many trailing items `a` and `b` share, by identity."
  [spec a b]
  (let [[m status] (aligned a b true)
        na (n a), nb (n b)
        c (min na nb)]
    (if (= status :lost)
      (loop [k m]
        (if (and (< k c) (identical? (get-item spec a (- na 1 k)) (get-item spec b (- nb 1 k))))
          (recur (inc k))
          k))
      (min m c))))
