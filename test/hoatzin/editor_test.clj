(ns hoatzin.editor-test
  (:require [clojure.test :refer [deftest is testing]]
            [hoatzin.editor :as ed]))

(deftest insert
  (testing "into an empty document"
    (is (= {:text "hi" :caret 2} (ed/insert ed/empty-doc "hi"))))
  (testing "at the caret, not the end"
    (is (= {:text "abXYc" :caret 4} (ed/insert {:text "abc" :caret 2} "XY"))))
  (testing "the caret advances by code points"
    (is (= {:text "a😀" :caret 2} (ed/insert {:text "a" :caret 1} "😀")))))

(deftest delete
  (is (= {:text "ac" :caret 1} (ed/delete {:text "abc" :caret 2} 1 2)))
  (testing "range ends in either order"
    (is (= (ed/delete {:text "abcdef" :caret 0} 1 4)
           (ed/delete {:text "abcdef" :caret 0} 4 1))))
  (testing "an empty range changes nothing but the caret"
    (is (= {:text "abc" :caret 1} (ed/delete {:text "abc" :caret 3} 1 1)))))

(deftest move
  (is (= {:text "abc" :caret 0} (ed/move {:text "abc" :caret 3} 0)))
  (testing "drops the selection"
    (is (= {:text "abc" :caret 1} (ed/move {:text "abc" :caret 3 :anchor 0} 1)))))

(deftest select
  (let [doc {:text "abcdef" :caret 2}]
    (is (nil? (ed/selection doc)) "nothing selected to begin with")
    (testing "extends from the caret"
      (let [doc (ed/select doc 5)]
        (is (= {:text "abcdef" :caret 5 :anchor 2} doc))
        (is (= [2 5] (ed/selection doc)))
        (is (= "cde" (ed/selected-text doc)))))
    (testing "keeps its anchor as the caret moves on, either side of it"
      (let [doc (-> doc (ed/select 5) (ed/select 0))]
        (is (= [0 2] (ed/selection doc)))
        (is (= "ab" (ed/selected-text doc)))))
    (testing "back to the anchor is no selection"
      (is (= doc (-> doc (ed/select 4) (ed/select 2)))))
    (testing "select all"
      (is (= [0 6] (ed/selection (ed/select-all doc))))
      (is (nil? (ed/selection (ed/select-all ed/empty-doc))) "of nothing selects nothing"))))

(deftest insert-replaces-the-selection
  (is (= {:text "aXf" :caret 2} (ed/insert {:text "abcdef" :caret 5 :anchor 1} "X")))
  (is (= {:text "aXf" :caret 2} (ed/insert {:text "abcdef" :caret 1 :anchor 5} "X"))
      "whichever end the caret is at"))

(deftest delete-drops-the-selection
  (is (= {:text "af" :caret 1} (ed/delete {:text "abcdef" :caret 5 :anchor 1} 1 5))))

(deftest word-range
  (let [text "the  hoatzin, a\tbird\n\nend"]
    (is (= [0 3] (ed/word-range text 0)) "a word, from its first character")
    (is (= [0 3] (ed/word-range text 2)) "from its last")
    (is (= [3 5] (ed/word-range text 3)) "a run of whitespace")
    (is (= [5 13] (ed/word-range text 9)) "punctuation is part of a word")
    (is (= [15 16] (ed/word-range text 15)) "tabs are whitespace")
    (is (= [16 20] (ed/word-range text 17)) "a newline ends a word")
    (is (= [22 25] (ed/word-range text 24)))
    (is (= [2 4] (ed/word-range "a 😀😀 b" 3)) "counting code points")))

(deftest select-word
  (is (= {:text "one two" :anchor 4 :caret 7} (ed/select-word {:text "one two" :caret 0} 5))))
