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
  (is (= {:text "abc" :caret 0} (ed/move {:text "abc" :caret 3} 0))))
