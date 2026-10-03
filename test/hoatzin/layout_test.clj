(ns hoatzin.layout-test
  "Layout against real CoreText. Exact line breaks depend on Georgia's
  metrics, so these assert properties (lines rejoin to the text, fit the
  width, break at spaces) rather than particular break positions."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hoatzin.coretext :as ct]
            [hoatzin.layout :as layout]))

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
          (let [[x _] (layout/caret L (layout/line-end L k))]
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
        (is (= 0 (second (layout/caret L (layout/line-end L 0))))
            "the end of a wrapped line stays on that line")
        (is (= start-1 (layout/position-at L 1 0.0)))))))

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

(deftest paragraph-cache
  (let [ctx (layout/context *font* 300)]
    (try
      (layout/layout ctx "alpha\nbeta\ngamma\n\n")
      (let [before @(:cache ctx)]
        (is (= #{"alpha" "beta" "gamma" ""} (set (keys before)))
            "identical paragraphs share one entry")
        (layout/layout ctx "alpha\nBETA\ngamma\n\n")
        (let [after @(:cache ctx)]
          (is (identical? (before "alpha") (after "alpha")) "untouched paragraphs are reused")
          (is (identical? (before "gamma") (after "gamma")))
          (is (not (contains? after "beta")) "paragraphs no longer present are evicted")
          (is (contains? after "BETA"))))
      (finally (layout/release-context ctx)))))
