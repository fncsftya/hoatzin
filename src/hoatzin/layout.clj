(ns hoatzin.layout
  "Document layout: the text split into paragraphs, each wrapped by CoreText.

  Positions are code-point indices into the document string, as Jolt
  strings count them; CoreText's UTF-16 indices stay inside this namespace.

  Paragraphs are cached by their text within a layout context (one font and
  wrap width), so an edit re-typesets only the paragraph it touched. Identical
  paragraphs (blank lines, say) share one entry. A layout pass evicts the
  paragraphs that no longer appear."
  (:require [clojure.string :as str]
            [hoatzin.coretext :as ct]))

;; ---------------------------------------------------------------- metrics

(defn metrics
  "Fixed per-font line metrics, in whole pixels so lines land on the grid."
  [{:keys [ascent descent leading]}]
  (let [a  (long (Math/ceil ascent))
        d  (long (Math/ceil descent))
        lh (long (Math/ceil (* 1.3 (+ a d (max 0.0 leading)))))
        gap (quot (- lh a d) 2)]
    {:line-height  lh
     :baseline     (+ gap a)           ; from the line's top
     :caret-top    gap
     :caret-height (+ a d)}))

;; ---------------------------------------------------------------- UTF-16

(defn- astral? [c] (>= (int c) 0x10000))

(defn- u16-table
  "For text outside the BMP, a vector mapping code-point index -> UTF-16
  index (n+1 entries). nil when the two coincide, which is the common case."
  [s]
  (when (some astral? s)
    (vec (reductions + 0 (map #(if (astral? %) 2 1) s)))))

(defn- cp->u16 [{:keys [u16]} i] (if u16 (nth u16 i) i))

(defn- u16->cp [{:keys [u16]} u]
  (if-not u16
    u
    (loop [lo 0, hi (dec (count u16))]       ; the last i with u16[i] <= u
      (if (>= lo hi)
        lo
        (let [mid (quot (+ lo hi 1) 2)]
          (if (<= (nth u16 mid) u) (recur mid hi) (recur lo (dec mid))))))))

;; ---------------------------------------------------------------- paragraphs

(defn- paragraph [font width text]
  (let [{:keys [string lines]} (ct/typeset font text width)
        p {:text text :string string :u16 (u16-table text)}]
    (assoc p :lines
           (mapv (fn [{:keys [start end] :as ln}]
                   (assoc ln :text (subs text (u16->cp p start) (u16->cp p end))))
                 lines))))

(defn- split-paragraphs [text]
  (loop [from 0, acc []]
    (if-let [i (str/index-of text "\n" from)]
      (recur (inc i) (conj acc (subs text from i)))
      (conj acc (subs text from)))))

(defn context
  "A layout context: `font` (from hoatzin.coretext/font) wrapped to `width` px."
  [font width]
  {:font font :width width :metrics (metrics font) :cache (atom {})})

(defn release-context [{:keys [cache]}]
  (run! ct/release-paragraph (vals @cache))
  (reset! cache {}))

(defn layout
  "Lay out `text` in `ctx`, reusing cached paragraphs.

  Returns {:paras [{:p :start :end :first-line}] :lines [[para line]] :metrics}."
  [{:keys [font width metrics cache]} text]
  (let [texts (split-paragraphs text)
        old   @cache
        new   (reduce (fn [m t]
                        (if (contains? m t)
                          m
                          (assoc m t (or (get old t) (paragraph font width t)))))
                      {} texts)]
    (doseq [[t p] old :when (not (contains? new t))]
      (ct/release-paragraph p))
    (reset! cache new)
    (loop [[t & more] texts, start 0, paras [], lines []]
      (if (nil? t)
        {:paras paras :lines lines :metrics metrics}
        (let [p (get new t)
              i (count paras)]
          (recur more
                 (+ start (count t) 1)
                 (conj paras {:p p :start start :end (+ start (count t))
                              :first-line (count lines)})
                 (into lines (map (fn [j] [i j])) (range (count (:lines p))))))))))

;; ---------------------------------------------------------------- queries

(defn line-count [L] (count (:lines L)))
(defn line-height [L] (get-in L [:metrics :line-height]))

(defn visual-line
  "Visual line `k`: {:line ctline :text s}."
  [L k]
  (let [[i j] (nth (:lines L) k)]
    (nth (get-in L [:paras i :p :lines]) j)))

(defn- para-index
  "The paragraph holding document position `pos`."
  [{:keys [paras]} pos]
  (loop [lo 0, hi (dec (count paras))]
    (if (>= lo hi)
      lo
      (let [mid (quot (+ lo hi 1) 2)]
        (if (<= (:start (nth paras mid)) pos) (recur mid hi) (recur lo (dec mid)))))))

(defn- line-index
  "The line of paragraph `p` holding UTF-16 index `u`: the last one starting
  at or before it, so a position at a wrap point belongs to the next line."
  [p u]
  (let [lines (:lines p)]
    (loop [j (dec (count lines))]
      (if (and (pos? j) (> (:start (nth lines j)) u)) (recur (dec j)) j))))

(defn caret
  "Where the caret for `pos` goes: [x-pixels visual-line]."
  [L pos]
  (let [i (para-index L pos)
        {:keys [p start first-line]} (nth (:paras L) i)
        u (cp->u16 p (- pos start))
        j (line-index p u)]
    [(ct/offset-for-index (:line (nth (:lines p) j)) u) (+ first-line j)]))

(defn position-at
  "The document position nearest pixel offset `x` on visual line `k`."
  [L k x]
  (let [[i j] (nth (:lines L) k)
        {:keys [p start]} (nth (:paras L) i)
        {:keys [lines string]} p
        {lo :start hi :end line :line} (nth lines j)
        u (-> (or (ct/index-for-position line x) lo) (max lo) (min hi))
        ;; The end of a wrapped line is the start of the next one; stay on
        ;; this line by stopping before its last character.
        u (if (and (= u hi) (> hi lo) (< j (dec (count lines))))
            (first (ct/composed-range string (dec u)))
            u)]
    (+ start (u16->cp p u))))

(defn range-segments
  "The visual extent of positions [a, b): a [k x0 x1] for each visual line
  the range covers, with x0 < x1 in pixels."
  [L a b]
  (for [{:keys [p start end first-line]} (:paras L)
        :when (and (< a end) (> b start))
        :let [ua (cp->u16 p (- (max a start) start))
              ub (cp->u16 p (- (min b end) start))]
        [j {:keys [line] ls :start le :end}] (map-indexed vector (:lines p))
        :when (and (< ua le) (> ub ls))
        :let [x0 (ct/offset-for-index line (max ua ls))
              x1 (ct/offset-for-index line (min ub le))]
        :when (< x0 x1)]
    [(+ first-line j) x0 x1]))

(defn line-start [L k] (position-at L k -1.0e9))
(defn line-end   [L k] (position-at L k 1.0e9))

(defn prev-position
  "The position one user-perceived character before `pos`."
  [L pos]
  (let [{:keys [p start]} (nth (:paras L) (para-index L pos))]
    (if (= pos start)
      (max 0 (dec pos))                      ; across the newline
      (let [[a _] (ct/composed-range (:string p) (dec (cp->u16 p (- pos start))))]
        (+ start (u16->cp p a))))))

(defn next-position
  "The position one user-perceived character after `pos`."
  [L pos]
  (let [i (para-index L pos)
        {:keys [p start end]} (nth (:paras L) i)]
    (cond
      (< pos end) (let [[_ b] (ct/composed-range (:string p) (cp->u16 p (- pos start)))]
                    (+ start (u16->cp p b)))
      (< i (dec (count (:paras L)))) (inc pos)   ; across the newline
      :else pos)))
