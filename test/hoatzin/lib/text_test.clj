(ns hoatzin.lib.text-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hoatzin.lib.text :as text]))

(deftest basics
  (let [t (text/of "one\ntwo\n\nfour")]
    (is (= "one\ntwo\n\nfour" (str t)))
    (is (= 13 (count t)))
    (is (= 4 (text/line-count t)))
    (is (= "two" (text/line t 1)))
    (is (= 9 (text/line-start t 3)))
    (is (= [0 0 "one"] (text/line-at t 3)) "a line's end belongs to it")
    (is (= [1 4 "two"] (text/line-at t 4)))
    (is (= [3 9 "four"] (text/line-at t 13)) "as does the text's end")
    (is (= \newline (text/char-at t 3)))
    (is (= "e\ntwo\n\nf" (text/slice t 2 10)))
    (is (= (text/of "one\ntwo\n\nfour") t) "equal by content")
    (is (not= (text/of "one\ntwo\n\nfive") t)))
  (testing "empty"
    (is (= "" (str text/empty-text)))
    (is (= 0 (count text/empty-text)))
    (is (= 1 (text/line-count text/empty-text)))
    (is (= [0 0 ""] (text/line-at text/empty-text 0))))
  (testing "replace"
    (let [t (text/of "abc\ndef")]
      (is (= "aXYc\ndef" (str (text/replace t 1 2 "XY"))))
      (is (= "abef" (str (text/replace t 2 5 ""))) "across a newline")
      (is (= "ab\n\ncdef" (str (text/replace t 2 4 "\n\nc"))))
      (is (= "" (str (text/replace t 0 7 "")))))))

(defn- rng [seed]
  (let [s (atom seed)]
    (fn [n] (swap! s #(mod (+ (* % 1103515245) 12345) 2147483648))
      (mod (quot @s 65536) n))))

(defn- random-string [r n]
  (apply str (repeatedly n #(nth "ab \n😀é" (r 6)))))

(deftest against-strings
  ;; random edits, small and large, checked against the same edits on a string
  (let [r (rng 7)]
    (loop [k 0, s "", t text/empty-text]
      (when (< k 400)
        (let [n (count s)
              lo (r (inc n))
              hi (min n (+ lo (if (zero? (r 4)) (r (inc n)) (r 3))))
              ins (random-string r (if (zero? (r 10)) (r 2000) (r 4)))
              s2 (str (subs s 0 lo) ins (subs s hi))
              t2 (text/replace t lo hi ins)]
          (is (= s2 (str t2)) (str "edit " k))
          (is (= (count s2) (count t2)))
          (is (= (count (str/split s2 #"\n" -1)) (text/line-count t2)))
          (when (pos? (count s2))
            (let [a (r (count s2)), b (+ a (r (- (inc (count s2)) a)))]
              (is (= (subs s2 a b) (text/slice t2 a b)))
              (is (= (nth s2 a) (text/char-at t2 a)))))
          (let [[i ja jb] (text/changed-lines t t2)
                old (text/lines t), new (text/lines t2)]
            (is (= (subvec old 0 i) (subvec new 0 i)))
            (is (= (subvec old ja) (subvec new jb)))
            (is (<= (- jb i) (inc (count (filter #{\newline} ins))))
                "only the lines the edit touched differ"))
          (recur (inc k) s2 t2))))))

(deftest large
  (let [s (str/join "\n" (map #(str "line " %) (range 100000)))
        t (text/of s)]
    (is (= s (str t)))
    (is (= "line 54321" (text/line t 54321)))
    (let [pos (text/line-start t 54321)
          t2 (text/insert t pos "x")]
      (is (= "xline 54321" (text/line t2 54321)))
      (is (= [54321 54322 54322] (text/changed-lines t t2)))
      (testing "deleting most of it"
        (let [t3 (text/replace t2 5 (- (count t2) 5) "")]
          (is (= (str (subs s 0 5) (subs s (- (count s) 5))) (str t3)))
          (is (= [0 100000 1] (text/changed-lines t2 t3))))))))

(deftest equality-across-edits
  (let [t (text/of (str/join "\n" (map #(str "line " %) (range 2000))))
        typed (text/insert t 5000 "x")
        undone (text/replace typed 5000 5001 "")]
    (is (= t undone) "an edit undone by hand is the same text")
    (is (let [[i] (text/line-at t 5000)]
          (not (identical? (text/line t i) (text/line undone i))))
        "though the edited line is a new string")
    (is (not= t typed))
    (is (not= t (text/replace t 5000 5001 "y")) "same length, different text")
    (is (not= t (text/replace t 5000 5000 "\n")) "different lines")
    (is (= (text/insert t 0 "a\nb") (text/of (str "a\nb" t))) "texts with no history in common")))
