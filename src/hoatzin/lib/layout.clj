(ns hoatzin.lib.layout
  "Document layout: the text's lines as paragraphs, each wrapped by CoreText.

  Positions are code-point indices into the document, as Jolt strings count
  them; CoreText's UTF-16 indices stay inside this namespace.

  The paragraphs are the items of a hoatzin.lib.tree, summing each one's length
  (with its newline) and its count of visual lines, so finding the paragraph
  at a position or a visual line is O(log n). A layout context (one font and
  wrap width) keeps the last layout it made, and the next one re-wraps only
  the lines that changed since: see hoatzin.lib.text/changed-lines.

  A paragraph is plain data, {:text :length :u16 :lines}, its lines just
  where it breaks. The CTLines that measure and draw lines are set only for
  the lines asked about, mostly those on screen, and cached in the context
  by their text until `trim!` finds them unused."
  (:require [hoatzin.lib.coretext :as ct]
            [hoatzin.lib.text :as text]
            [hoatzin.lib.tree :as tree]))

;; ---------------------------------------------------------------- metrics

(def line-spacing "Line height, as a multiple of the font's." 1.3)

(defn metrics
  "Fixed per-font line metrics, in whole pixels so lines land on the grid."
  [{:keys [ascent descent leading]}]
  (let [a  (long (Math/ceil ascent))
        d  (long (Math/ceil descent))
        lh (long (Math/ceil (* line-spacing (+ a d (max 0.0 leading)))))
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

(def ^:private parallel-chars
  "Past this many characters, paragraphs are wrapped on several threads:
  wrapping a whole large text, as a new font or width needs, is CoreText's
  work, and its typesetters on different threads don't contend."
  100000)

(def ^:private threads (max 1 (.availableProcessors (Runtime/getRuntime))))

(defn- wrapped
  "Paragraphs for the strings `texts`, wrapped, on this thread."
  [font width texts]
  (mapv (fn [text {:keys [length lines]}]
          {:text text :length length :lines lines
           ;; UTF-16 is longer than the code points only past the BMP
           :u16 (when (not= length (count text)) (u16-table text))})
        texts (ct/wrap font texts width)))

(defn- paragraphs-of
  "Paragraphs for the strings `texts`, wrapped: many of them in runs of
  about the same number, one run to a thread."
  [font width texts]
  (let [texts (vec texts)
        n     (count texts)]
    (if (or (< n (* 2 threads)) (< (reduce + 0 (map count texts)) parallel-chars))
      (wrapped font width texts)
      (let [per (quot (+ n threads -1) threads)]
        (into [] (mapcat deref)
              (mapv #(future (wrapped font width (subvec texts % (min n (+ % per)))))
                    (range 0 n per)))))))

(defn- edit-range
  "Where string `b` differs from `a`: [p ea eb], a's [p, ea) having become
  b's [p, eb)."
  [^String a ^String b]
  ;; .charAt on a hinted string is several times faster than nth
  (let [na (.length a), nb (.length b), m (min na nb)
        p (loop [k 0] (if (and (< k m) (= (.charAt a k) (.charAt b k))) (recur (inc k)) k))
        s (loop [k 0]
            (if (and (< k (- m p)) (= (.charAt a (- na 1 k)) (.charAt b (- nb 1 k))))
              (recur (inc k))
              k))]
    [p (- na s) (- nb s)]))

(defn- rewrap
  "Paragraph `old` edited to `text`, re-breaking only the lines the edit
  disturbs (see hoatzin.lib.coretext/rewrap)."
  [font width old text]
  (let [[p ea eb] (edit-range (:text old) text)
        u16 (when (or (:u16 old) (some astral? (subs text p eb))) (u16-table text))
        new {:text text :u16 u16}
        {:keys [length lines]}
        (ct/rewrap font old text width (cp->u16 old p) (cp->u16 old ea) (cp->u16 new eb))]
    (assoc new :length length :lines lines)))

;; A paragraph's :len counts its newline; its :w is its visual lines.
(def ^:private spec {:len #(inc (count (:text %))) :w #(count (:lines %))})

(defn context
  "A layout context: `font` (from hoatzin.lib.coretext/font) wrapped to `width`
  px. Release it with `release-context`."
  [font width]
  {:font font :width width :metrics (metrics font) :current (atom nil)
   ;; CTLines by their text, each with the generation that last used it
   :ctlines (atom {:gen 0 :entries {}})})

(def ^:private spare-lines
  "CTLines kept past those in use, most recently used first."
  1024)

(def ^:private slack-lines
  "How many more than `spare-lines` may build up before `trim!` evicts."
  256)

(defn trim!
  "Release cached CTLines unused since the last trim, keeping the
  `spare-lines` most recently used. Call it between frames: a CTLine that a
  query hands out is good until the next trim."
  [{:keys [ctlines]}]
  (let [{:keys [gen entries]} @ctlines]
    (when (> (count entries) (+ spare-lines slack-lines))
      (let [stale (->> entries
                       (remove (fn [[_ e]] (= gen (:used e))))
                       (sort-by (fn [[_ e]] (- (:used e))))
                       (drop spare-lines))]
        (doseq [[_ e] stale] (ct/release (:line e)))
        (swap! ctlines update :entries #(apply dissoc % (map key stale)))))
    (swap! ctlines update :gen inc)))

(defn release-context [{:keys [current ctlines]}]
  (doseq [[_ e] (:entries @ctlines)] (ct/release (:line e)))
  (reset! ctlines {:gen 0 :entries {}})
  (reset! current nil))

(defn layout
  "Lay out `txt` (a hoatzin.lib.text, or a string) in `ctx`, re-wrapping only
  the lines that differ from the context's last layout.

  Returns {:tree :text :metrics :font :ctlines}; the functions below answer
  questions of it."
  [{:keys [font width metrics current ctlines]} txt]
  (let [txt  (text/of txt)
        old  @current
        fresh #(tree/build spec (paragraphs-of font width (text/lines txt)))
        t    (if (nil? old)
               (fresh)
               (let [[i ja jb] (text/changed-lines (:text old) txt)
                     t (:tree old)]
                 (cond
                   (= i ja jb) t
                   ;; one line edited, as typing does: re-break just around the edit
                   (= (inc i) ja jb)
                   (tree/splice spec t i ja [(rewrap font width (tree/get-item spec t i)
                                                     (text/line txt i))])
                   ;; a new text altogether, as opening a file gives
                   (and (zero? i) (= ja (tree/n t))) (fresh)
                   :else
                   (tree/splice spec t i ja (paragraphs-of font width (text/lines txt i jb))))))
        L    {:tree t :text txt :metrics metrics :font font :ctlines ctlines}]
    (reset! current L)
    L))

;; ---------------------------------------------------------------- queries

(defn line-count [L] (tree/w (:tree L)))
(defn paragraphs
  "Every paragraph, as {:text :length :u16 :lines}: for tests."
  [L]
  (tree/items spec (:tree L)))
(defn line-height [L] (get-in L [:metrics :line-height]))

(defn- para
  "The paragraph where the running sum of `dim` passes `x`, as
  {:p :i :start :end :first-line}: by :len, the one holding document
  position x; by :w, the one holding visual line x."
  [L dim x]
  (let [[p i start first-line] (tree/locate spec (:tree L) dim x)]
    {:p p :i i :start start :end (+ start (count (:text p))) :first-line first-line}))

(defn- para-at [L pos] (para L :len pos))
(defn- para-of-line [L k] (para L :w k))

(defn- line-ref
  "Visual line `k` as [paragraph line-within-it]."
  [L k]
  (let [{:keys [first-line] :as pa} (para-of-line L k)]
    [pa (- k first-line)]))

(defn- line-text
  "The text of line `ln` of paragraph `p`. It is cut from the paragraph's
  when wanted, not kept: most paragraphs are one line, and keeping a copy
  would double the memory a document takes."
  [p {:keys [start end]}]
  (let [text (:text p)]
    (if (and (zero? start) (= end (:length p)))
      text
      (subs text (u16->cp p start) (u16->cp p end)))))

(defn- ctline
  "The CTLine of line text `s`, from the cache or set now; nil for \"\"."
  [{:keys [font ctlines]} s]
  (when (seq s)
    (let [{:keys [gen entries]} @ctlines]
      (if-let [e (get entries s)]
        (do (when-not (= gen (:used e)) (swap! ctlines assoc-in [:entries s :used] gen))
            (:line e))
        (let [line (ct/make-line font s)]
          (swap! ctlines assoc-in [:entries s] {:line line :used gen})
          line)))))

(defn- set-line
  "Line `j` of paragraph `p`, ready to measure: {:start :end :text, :line
  its CTLine and :base its start, as hoatzin.lib.coretext's line functions take
  it}. Good until the next `trim!`."
  [L p j]
  (let [{:keys [start] :as ln} (nth (:lines p) j)
        s (line-text p ln)]
    (assoc ln :text s :line (ctline L s) :base start)))

(defn visual-line
  "Visual line `k`: {:line ctline :text s}, good until the next `trim!`."
  [L k]
  (let [[{:keys [p]} j] (line-ref L k)]
    (set-line L p j)))

(defn- line-index
  "The line of paragraph `p` holding UTF-16 index `u`: the last one starting
  at or before it, so a position at a wrap point belongs to the next line."
  [p u]
  (let [lines (:lines p)]
    (loop [j (dec (count lines))]
      (if (and (pos? j) (> (:start (nth lines j)) u)) (recur (dec j)) j))))

(defn caret
  "Where the caret for `pos` goes: [x-pixels visual-line].

  Where a paragraph wraps, the end of one line and the start of the next are
  the same position. The caret goes at the start of the next line, or with
  `upstream?` at the end of the line before (after its trailing space): see
  `wrap-end?`."
  ([L pos] (caret L pos false))
  ([L pos upstream?]
   (let [{:keys [p start first-line]} (para-at L pos)
         u (cp->u16 p (- pos start))
         j (line-index p u)
         j (if (and upstream? (pos? j) (= u (:start (nth (:lines p) j)))) (dec j) j)]
     [(ct/offset-for-index (set-line L p j) u) (+ first-line j)])))

(defn last-line
  "The last visual line of the paragraph holding `pos`."
  [L pos]
  (let [{:keys [p first-line]} (para-at L pos)]
    (+ first-line (dec (count (:lines p))))))

(defn wrap-end?
  "Whether `pos` is the end of visual line `k` where its paragraph wraps
  onto the next line: a caret placed there for line `k` belongs upstream."
  [L k pos]
  (let [[{:keys [p start]} j] (line-ref L k)
        lines (:lines p)]
    (and (< j (dec (count lines)))
         (= pos (+ start (u16->cp p (:end (nth lines j))))))))

(defn position-at
  "The document position nearest pixel offset `x` on visual line `k`. Past
  the end of a wrapped line that is the wrap point (see `wrap-end?`)."
  [L k x]
  (let [[{:keys [p start]} j] (line-ref L k)
        {lo :start hi :end :as ln} (set-line L p j)
        u (-> (or (ct/index-for-position ln x) lo) (max lo) (min hi))]
    (+ start (u16->cp p u))))

(defn char-at
  "The document index of the character under pixel offset `x` on visual
  line `k`, or nil on an empty line."
  [L k x]
  (let [[{:keys [p start]} j] (line-ref L k)
        {lo :start hi :end :as ln} (set-line L p j)
        pos (position-at L k x)
        u (cp->u16 p (- pos start))]
    (when (< lo hi)
      ;; position-at gives the nearest boundary; the character is the one
      ;; on the side of it that `x` falls.
      (if (and (> u lo) (or (= u hi) (< x (ct/offset-for-index ln u))))
        (dec pos)
        pos))))

(defn- paras-on-lines
  "The paragraphs with a visual line in [k0, k1), as `para` gives them."
  [L k0 k1]
  (let [k0 (max k0 0)
        k1 (min k1 (line-count L))]
    (if (< k0 k1)
      (tree/fold spec (:tree L) (:i (para-of-line L k0)) (inc (:i (para-of-line L (dec k1))))
                 (fn [acc p i start first-line]
                   (conj acc {:p p :i i :start start :end (+ start (count (:text p)))
                              :first-line first-line}))
                 [])
      [])))

(defn range-segments
  "The visual extent of positions [a, b): a [k x0 x1] for each visual line
  the range covers, with x0 < x1 in pixels. Given [k0, k1), only for the
  visual lines in that window, so the cost follows what is on screen rather
  than the length of the range."
  ([L a b] (range-segments L a b 0 (line-count L)))
  ([L a b k0 k1]
   (for [{:keys [p start end first-line]} (paras-on-lines L k0 k1)
         :when (and (< a end) (> b start))
         :let [ua (cp->u16 p (- (max a start) start))
               ub (cp->u16 p (- (min b end) start))]
         :let [lines (:lines p)]
         j (range (max 0 (- k0 first-line)) (min (count lines) (- k1 first-line)))
         :let [{ls :start le :end} (nth lines j)
               k (+ first-line j)]
         :when (and (< ua le) (> ub ls))
         :let [ln (set-line L p j)]
         :let [x0 (ct/offset-for-index ln (max ua ls))
               x1 (ct/offset-for-index ln (min ub le))]
         :when (< x0 x1)]
     [k x0 x1])))

(defn selection-segments
  "Like `range-segments`, plus a `newline-width` px box after the last line
  of each paragraph whose newline the range covers, so a selection shows
  where it crosses paragraph ends and blank lines."
  ([L a b newline-width] (selection-segments L a b newline-width 0 (line-count L)))
  ([L a b newline-width k0 k1]
   (let [text-end (dec (tree/len (:tree L)))]  ; the last paragraph has no newline
     (sort (concat (range-segments L a b k0 k1)
                   (for [{:keys [p end first-line]} (paras-on-lines L k0 k1)
                         :let [k (+ first-line (dec (count (:lines p))))]
                         :when (and (<= a end) (< end b) (< end text-end) (<= k0 k) (< k k1))
                         :let [ln (set-line L p (dec (count (:lines p))))
                               x (ct/offset-for-index ln (:end ln))]]
                     [k x (+ x newline-width)]))))))

(defn line-start [L k] (position-at L k -1.0e9))
(defn line-end   [L k] (position-at L k 1.0e9))

(defn prev-position
  "The position one user-perceived character before `pos`."
  [L pos]
  (let [{:keys [p start]} (para-at L pos)]
    (if (= pos start)
      (max 0 (dec pos))                      ; across the newline
      (let [[a _] (ct/composed-range (:text p) (dec (cp->u16 p (- pos start))))]
        (+ start (u16->cp p a))))))

(defn next-position
  "The position one user-perceived character after `pos`."
  [L pos]
  (let [{:keys [p i start end]} (para-at L pos)]
    (cond
      (< pos end) (let [[_ b] (ct/composed-range (:text p) (cp->u16 p (- pos start)))]
                    (+ start (u16->cp p b)))
      (< i (dec (tree/n (:tree L)))) (inc pos)   ; across the newline
      :else pos)))
