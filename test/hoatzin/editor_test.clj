(ns hoatzin.editor-test
  (:require [clojure.test :refer [deftest is testing]]
            [hoatzin.editor :as ed]))

(defn- doc
  "A document, as the tests write them: {:text string :caret i :anchor j}."
  [{:keys [text] :as m}]
  (assoc m :text (:text (ed/doc text))))

(defn- plain
  "A document with its text as a string, to compare."
  [d]
  (update d :text str))

(deftest insert
  (testing "into an empty document"
    (is (= {:text "hi" :caret 2} (plain (ed/insert ed/empty-doc "hi")))))
  (testing "at the caret, not the end"
    (is (= {:text "abXYc" :caret 4} (plain (ed/insert (doc {:text "abc" :caret 2}) "XY")))))
  (testing "the caret advances by code points"
    (is (= {:text "a😀" :caret 2} (plain (ed/insert (doc {:text "a" :caret 1}) "😀"))))))

(deftest delete
  (is (= {:text "ac" :caret 1} (plain (ed/delete (doc {:text "abc" :caret 2}) 1 2))))
  (testing "range ends in either order"
    (is (= (ed/delete (doc {:text "abcdef" :caret 0}) 1 4)
           (ed/delete (doc {:text "abcdef" :caret 0}) 4 1))))
  (testing "an empty range changes nothing but the caret"
    (is (= {:text "abc" :caret 1} (plain (ed/delete (doc {:text "abc" :caret 3}) 1 1))))))

(deftest move
  (is (= {:text "abc" :caret 0} (plain (ed/move (doc {:text "abc" :caret 3}) 0))))
  (testing "drops the selection"
    (is (= {:text "abc" :caret 1} (plain (ed/move (doc {:text "abc" :caret 3 :anchor 0}) 1))))))

(deftest select
  (let [doc (doc {:text "abcdef" :caret 2})]
    (is (nil? (ed/selection doc)) "nothing selected to begin with")
    (testing "extends from the caret"
      (let [doc (ed/select doc 5)]
        (is (= {:text "abcdef" :caret 5 :anchor 2} (plain doc)))
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
  (is (= {:text "aXf" :caret 2} (plain (ed/insert (doc {:text "abcdef" :caret 5 :anchor 1}) "X"))))
  (is (= {:text "aXf" :caret 2} (plain (ed/insert (doc {:text "abcdef" :caret 1 :anchor 5}) "X")))
      "whichever end the caret is at"))

(deftest delete-drops-the-selection
  (is (= {:text "af" :caret 1} (plain (ed/delete (doc {:text "abcdef" :caret 5 :anchor 1}) 1 5)))))

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
  (is (= {:text "one two" :anchor 4 :caret 7} (plain (ed/select-word (doc {:text "one two" :caret 0}) 5)))))

(deftest marks
  (let [d (-> (doc {:text "abcdef" :caret 2}) (ed/mark :m 2) (ed/mark :n 4))]
    (testing "text typed at a mark goes after it; before one, pushes it on"
      (is (= {:m 2 :n 6} (:marks (ed/insert d "XY")))))
    (testing "deleting around a mark leaves it where the deletion was"
      (is (= {:m 1 :n 1} (:marks (ed/delete d 1 5)))))
    (testing "deleting before one pulls it back"
      (is (= {:m 0 :n 2} (:marks (ed/delete d 0 2)))))
    (testing "unmarking the last mark leaves no :marks"
      (is (= {:text "abcdef" :caret 2} (plain (-> d (ed/unmark :m) (ed/unmark :n))))))))
