(ns hoatzin.lib.layout-test
  "Layout against real CoreText. Exact line breaks depend on Georgia's
  metrics, so these assert properties (lines rejoin to the text, fit the
  width, break at spaces) rather than particular break positions."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hoatzin.lib.coretext :as ct]
            [hoatzin.lib.layout :as layout]
            [hoatzin.lib.text :as text]))

(def ^:dynamic *font* nil)

(use-fixtures :once
  (fn [t]
    (let [f (ct/font 40)]
      (try (binding [*font* f] (t))
           (finally (ct/release-font f))))))

(defn- with-layout
  "Lay `text` out at `width` px and call (f L ctx)."
  [width text f]
  (let [ctx (layout/context *font* width)]
    (try (f (layout/layout ctx text) ctx)
         (finally (layout/release-context ctx)))))

(defn- line-texts [L]
  (mapv #(:text (layout/visual-line L %)) (range (layout/line-count L))))

(def sentence "The hoatzin is a tropical bird found in the swamps of the Amazon basin.")

(deftest font-and-metrics
  (is (= "Georgia" (:family *font*)) "the default serif face is available")
  (let [{:keys [line-height baseline caret-top caret-height]} (layout/metrics *font*)]
    (is (every? integer? [line-height baseline caret-top caret-height]))
    (is (< caret-top baseline line-height))
    (is (<= (+ caret-top caret-height) line-height))))

(deftest empty-document
  (with-layout 300 ""
    (fn [L _]
      (is (= 1 (layout/line-count L)))
      (is (= [0.0 0] (layout/caret L 0)))
      (is (= 0 (layout/next-position L 0)))
      (is (= 0 (layout/prev-position L 0))))))

(deftest wrapping
  (with-layout 300 sentence
    (fn [L _]
      (let [lines (line-texts L)]
        (is (< 1 (count lines)) "a long sentence wraps")
        (is (= sentence (apply str lines)) "the lines rejoin to the paragraph")
        (is (every? #(str/ends-with? % " ") (butlast lines)) "breaks fall after spaces")
        (doseq [k (range (count lines))]
          ;; trailing whitespace may hang past the width; the rest may not
          (let [[x _] (layout/caret L (- (layout/line-end L k)
                                         (count (re-find #"\s*$" (nth lines k)))))]
            (is (<= x 300) (str "line " k " fits the wrap width"))))))))

(deftest wider-means-fewer-lines
  (let [count-at (fn [w] (with-layout w sentence (fn [L _] (layout/line-count L))))]
    (is (> (count-at 200) (count-at 400) (count-at 2000)))
    (is (= 1 (count-at 2000)))))

(deftest paragraphs
  (with-layout 300 "one\n\nthree"
    (fn [L _]
      (is (= ["one" "" "three"] (line-texts L)))
      (is (= [0 0] (mapv int (layout/caret L 0))))
      (is (= 1 (second (layout/caret L 4))) "after the newline is the blank line")
      (is (= 2 (second (layout/caret L 5))))
      (testing "moving across newlines"
        (is (= 4 (layout/next-position L 3)))
        (is (= 3 (layout/prev-position L 4)))))))

(deftest wrap-points
  (with-layout 300 sentence
    (fn [L _]
      (let [start-1 (layout/line-start L 1)]
        (is (= 1 (second (layout/caret L start-1)))
            "a position at a wrap point is drawn at the start of the next line")
        (is (= start-1 (layout/line-end L 0))
            "the end of a wrapped line is the start of the next")
        (is (= start-1 (layout/position-at L 0 1.0e9)) "as is clicking past it")
        (is (= start-1 (layout/position-at L 1 0.0)))
        (testing "upstream, the caret is drawn at the end of the line"
          (let [[x k] (layout/caret L start-1 true)
                [before-space _] (layout/caret L (dec start-1))]
            (is (= 0 k))
            (is (> x before-space) "after the trailing space")))
        (is (= (layout/caret L 4) (layout/caret L 4 true))
            "affinity only matters at a wrap point")
        (is (= [0.0 0] (layout/caret L 0 true)) "or the start of a paragraph")
        (testing "wrap-end?"
          (is (layout/wrap-end? L 0 start-1))
          (is (not (layout/wrap-end? L 1 start-1)) "the same position, for the next line")
          (is (not (layout/wrap-end? L 0 (dec start-1))))
          (let [last-k (dec (layout/line-count L))]
            (is (not (layout/wrap-end? L last-k (layout/line-end L last-k)))
                "the paragraph's end is no wrap point")))))))

(deftest char-at
  (with-layout 2000 "ab cd\n\nx"
    (fn [L _]
      (let [x #(first (layout/caret L %))
            mid #(/ (+ (x %) (x (inc %))) 2.0)]
        (is (= 0 (layout/char-at L 0 -50.0)) "before the line is its first character")
        (is (= 0 (layout/char-at L 0 (mid 0))))
        (is (= 1 (layout/char-at L 0 (- (x 2) 0.5))) "either side of a boundary")
        (is (= 2 (layout/char-at L 0 (+ (x 2) 0.5))))
        (is (= 4 (layout/char-at L 0 5000.0)) "past the end is the last character")
        (is (nil? (layout/char-at L 1 0.0)) "nothing on a blank line")
        (is (= 7 (layout/char-at L 2 5000.0)))))))

(deftest grapheme-clusters
  ;; 👍🏽 is two code points (thumb + skin tone); é here is e + U+0301.
  (let [text "a👍🏽éb"]
    (with-layout 2000 text
      (fn [L _]
        (is (= [0 1 3 5 6] (take 5 (iterate #(layout/next-position L %) 0)))
            "next-position steps over whole user-perceived characters")
        (is (= [6 5 3 1 0] (take 5 (iterate #(layout/prev-position L %) 6))))
        (is (= 6 (layout/next-position L 6)) "stops at the end")))))

(deftest astral-characters-map-to-utf16
  ;; 😀 is one code point but two UTF-16 units; positions after it must
  ;; still land on the right glyphs.
  (with-layout 2000 "😀😀ab"
    (fn [L _]
      (let [xs (mapv #(first (layout/caret L %)) (range 5))]
        (is (apply < xs) "each position is further right than the last")
        (doseq [pos (range 5)]
          (is (= pos (layout/position-at L 0 (nth xs pos)))
              (str "position " pos " round-trips through its x")))))))

(deftest caret-hit-testing-round-trips
  (with-layout 300 sentence
    (fn [L _]
      (doseq [pos (range (inc (count sentence)))
              :let [[x k] (layout/caret L pos)
                    wrap-point? (and (pos? k) (= pos (layout/line-start L k)))]]
        (is (= pos (layout/position-at L k x)) (str "position " pos))
        (when wrap-point?
          (is (zero? x)))))))

(deftest range-segments
  (with-layout 300 sentence
    (fn [L _]
      (testing "a range within one line"
        (let [[[k x0 x1] :as segs] (layout/range-segments L 4 11)]   ; "hoatzin"
          (is (= 1 (count segs)))
          (is (= 0 k))
          (is (= [x0 x1] [(first (layout/caret L 4)) (first (layout/caret L 11))]))))
      (testing "a range across a wrap covers both lines"
        (let [end-0 (layout/line-start L 1)
              segs  (layout/range-segments L (- end-0 3) (+ end-0 3))]
          (is (= [0 1] (mapv first segs)))
          (is (zero? (second (second segs))) "the second segment starts at the margin")))
      (testing "an empty range covers nothing"
        (is (empty? (layout/range-segments L 5 5)))))))

(deftest selection-segments
  (with-layout 300 "one\n\nthree"
    (fn [L _]
      (testing "within a paragraph it is just the range"
        (is (= (layout/range-segments L 0 2) (layout/selection-segments L 0 2 5))))
      (testing "a covered newline adds a box at its line's end"
        (let [[x-end _] (layout/caret L 3)]
          (is (= [[0 x-end (+ x-end 5)]] (layout/selection-segments L 3 4 5)))))
      (testing "a blank line in the range shows as selected"
        (let [segs (layout/selection-segments L 1 7 5)]
          (is (= [0 0 1 2] (mapv first segs)) "text then newline on line 0")
          (is (= [1 0.0 5.0] (nth segs 2)))))
      (testing "stopping at a newline doesn't cover it"
        (is (= [0] (mapv first (layout/selection-segments L 0 3 5))))))))

(deftest segments-in-a-window-of-lines
  (let [text (str sentence "\n\n" sentence "\n" sentence)]
    (with-layout 300 text
      (fn [L _]
        (let [n    (layout/line-count L)
              all  (layout/selection-segments L 0 (count text) 5)
              in   (fn [k0 k1] (filterv #(< (dec k0) (first %) k1) all))]
          (is (< 6 n))
          (doseq [[k0 k1] [[0 n] [0 1] [2 5] [3 4] [(dec n) n] [-3 2] [5 (+ n 9)]]]
            (is (= (in k0 k1) (vec (layout/selection-segments L 0 (count text) 5 k0 k1)))
                (str "selection on lines [" k0 ", " k1 ")"))
            (is (= (filterv #(< (dec k0) (first %) k1) (layout/range-segments L 3 (- (count text) 3)))
                   (vec (layout/range-segments L 3 (- (count text) 3) k0 k1)))
                (str "range on lines [" k0 ", " k1 ")")))
          (is (empty? (layout/range-segments L 0 (count text) 4 4)) "an empty window"))))))

(deftest incremental-layout
  (let [ctx (layout/context *font* 300)
        t1 (text/of "alpha\nbeta\ngamma\n\n")]
    (try
      (let [before (layout/paragraphs (layout/layout ctx t1))
            t2 (text/replace t1 6 10 "BETA\nand delta")
            L (layout/layout ctx t2)
            after (layout/paragraphs L)]
        (is (= ["alpha" "BETA" "and delta" "gamma" "" ""] (mapv :text after)))
        (is (identical? (before 0) (after 0)) "untouched paragraphs are reused")
        (is (identical? (before 2) (after 3)))
        (is (identical? (before 4) (after 5)))
        (is (= 6 (layout/line-count L)))
        (is (= [0.0 3] (layout/caret L 21)) "and found where they moved to"))
      (testing "an unrelated text is laid out afresh"
        (let [L (layout/layout ctx "one\ntwo")]
          (is (= ["one" "two"] (mapv :text (layout/paragraphs L))))))
      (finally (layout/release-context ctx)))))

(defn- breaks
  "Where each paragraph of `L` breaks, as UTF-16 [start end] pairs."
  [L]
  (mapv (fn [p] (mapv (juxt :start :end) (:lines p))) (layout/paragraphs L)))

(defn- fresh-breaks [width txt]
  (with-layout width txt (fn [L _] (breaks L))))

(def ^:private long-paragraph
  (str/join " " (take 4000 (cycle ["the" "hoatzin" "is" "a" "tropical" "bird," "café" "😀"
                                   "found" "in" "swamps"]))))

(deftest long-paragraphs
  (testing "wrapping a window at a time breaks where wrapping it whole does"
    (let [whole (binding [ct/*window* 10000000] (fresh-breaks 300 long-paragraph))]
      (is (< 100 (count (first whole))))
      (is (= whole (fresh-breaks 300 long-paragraph)))))
  (testing "short paragraphs wrapped together break as they do alone"
    (let [paras ["The hoatzin is a tropical bird found in the swamps." "" "café 😀 naïve"
                 "short" ""]]
      (is (= (mapcat #(fresh-breaks 300 %) paras)
             (fresh-breaks 300 (str/join "\n" paras)))))))

(deftest rewrapping
  ;; random edits to one long paragraph, laid out incrementally in one
  ;; context, against laying each version out afresh
  (let [ctx (layout/context *font* 300)
        seed (atom 11)
        rand-int* (fn [n] (swap! seed #(mod (+ (* % 1103515245) 12345) 2147483648))
                    (mod (quot @seed 65536) n))]
    (try
      (layout/layout ctx long-paragraph)
      (loop [k 0, t (text/of long-paragraph)]
        (when (< k 60)
          (let [n (count t)
                L (layout/layout ctx t)
                ;; often at a line's start, where a word may move up a line
                at (if (even? k)
                     (layout/line-start L (rand-int* (layout/line-count L)))
                     (rand-int* (inc n)))
                hi (min n (+ at (rand-int* 6)))
                ins (nth ["" "x" " " "hoatzin " "😀" "a longer insertion of several words "] (rand-int* 6))
                t2 (text/replace t at hi ins)]
            (is (= (fresh-breaks 300 t2) (breaks (layout/layout ctx t2))) (str "edit " k))
            (recur (inc k) t2))))
      (finally (layout/release-context ctx)))))


(deftest incremental-matches-fresh
  ;; edits across paragraphs: newlines typed and deleted, ranges removed
  (let [ctx (layout/context *font* 300)
        seed (atom 5)
        rand-int* (fn [n] (swap! seed #(mod (+ (* % 1103515245) 12345) 2147483648))
                    (mod (quot @seed 65536) n))
        doc (str/join "\n" (map #(str sentence " " %) (range 40)))]
    (try
      (loop [k 0, t (text/of doc)]
        (when (< k 60)
          (let [n (count t)
                at (rand-int* (inc n))
                hi (min n (+ at (if (zero? (rand-int* 5)) (rand-int* 400) (rand-int* 4))))
                ins (nth ["" "\n" "x\ny" "\n\n" "bird " sentence] (rand-int* 6))
                t2 (text/replace t at hi ins)
                L (layout/layout ctx t2)]
            (is (= (fresh-breaks 300 t2) (breaks L)) (str "edit " k))
            (is (= (str t2) (str/join "\n" (map :text (layout/paragraphs L)))))
            (recur (inc k) t2))))
      (finally (layout/release-context ctx)))))

(deftest wrapping-many-paragraphs-on-several-threads
  ;; enough text to be split between threads; the breaks are the same
  (let [texts (vec (for [i (range 400)] (str/join " " (repeat (+ 20 (mod i 37)) "hoatzin café"))))]
    (is (= (#'layout/wrapped *font* 500 texts)
           (#'layout/paragraphs-of *font* 500 texts)))))
