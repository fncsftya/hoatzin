(ns hoatzin.app.journal-test
  (:require [clojure.test :refer [deftest is testing]]
            [hoatzin.app.journal :as journal]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.text :as text]
            [hoatzin.test-support :as t]))

(defn- applied
  "Text `a` with `ch`, a journal/change, made."
  [a {:keys [lo old-count new]}]
  (text/replace a lo (+ lo old-count) new))

(deftest changes
  (let [a (text/of "one\ntwo\nthree")]
    (is (nil? (journal/change a a)))
    (is (nil? (journal/change a (text/of "one\ntwo\nthree"))) "equal, not identical")
    (testing "within a line"
      (is (= {:lo 5 :old-count 0 :new "X"} (journal/change a (text/insert a 5 "X"))))
      (is (= {:lo 4 :old-count 3 :new ""} (journal/change a (text/replace a 4 7 "")))))
    (testing "lines come and go with their newlines"
      (is (= {:lo 4 :old-count 0 :new "new\n"} (journal/change a (text/insert a 4 "new\n"))))
      (is (= {:lo 0 :old-count 0 :new "zero\n"} (journal/change a (text/insert a 0 "zero\n"))))
      (is (= {:lo 13 :old-count 0 :new "\nfour"} (journal/change a (text/insert a 13 "\nfour"))))
      (is (= {:lo 3 :old-count 4 :new ""} (journal/change a (text/replace a 3 7 "")))))
    (testing "from and to nothing"
      (is (= {:lo 0 :old-count 13 :new ""} (journal/change a text/empty-text)))
      (is (= {:lo 0 :old-count 0 :new "one\ntwo\nthree"} (journal/change text/empty-text a))))))

(deftest changes-reproduce-the-text
  (let [rand-text (fn [r n] (apply str (repeatedly n #(rand-nth (if r "ab\n" "ab\ncd\n é")))))]
    (dotimes [_ 300]
      (let [a (text/of (rand-text true (rand-int 12)))
            n (count a)
            lo (rand-int (inc n))
            hi (+ lo (rand-int (inc (- n lo))))
            b (text/replace a lo hi (rand-text true (rand-int 5)))
            ch (journal/change a b)]
        (is (= (str b) (str (if ch (applied a ch) a))) (pr-str [(str a) lo hi (str b) ch]))))
    (testing "of texts with nothing shared"
      (dotimes [_ 100]
        (let [a (text/of (rand-text false (rand-int 12)))
              b (text/of (rand-text false (rand-int 12)))
              ch (journal/change a b)]
          (is (= (str b) (str (if ch (applied a ch) a))) (pr-str [(str a) (str b) ch])))))))

(defn- ops [s] (:journal @s))

(deftest keeping-edits
  (t/with-session [s]
    (is (= [] (ops s)) "nothing to lose, nothing kept")
    (t/type! s "a")
    (let [[base & more :as kept] (ops s)]
      (is (= 1 (count kept)) "the first edit is a base of the whole buffer")
      (is (= {:op :base :id 0 :gen 1 :text "a" :insets [] :caret 1
              :meta {:path nil :name "scratch" :scratch? true :dir nil :mode nil}}
             base))
      (is (empty? more)))
    (t/type! s "bc")
    (t/send! s {:type :key :key sdl/K-RETURN :mod 0})
    (is (= [{:op :edit :id 0 :gen 1 :lo 1 :old-count 0 :new "b" :caret 2}
            {:op :edit :id 0 :gen 1 :lo 2 :old-count 0 :new "c" :caret 3}
            {:op :edit :id 0 :gen 1 :lo 3 :old-count 0 :new "\n" :caret 4}]
           (rest (ops s))))
    (testing "an undo is an edit too"
      (t/press! s sdl/K-ESCAPE)
      (t/type! s "u")
      (is (= "abc" (t/text s)))
      (is (= {:op :edit :id 0 :gen 1 :lo 3 :old-count 1 :new "" :caret 3}
             (last (ops s)))))
    (testing "back where it began: nothing to lose"
      (t/type! s "u")
      (is (= "" (t/text s)))
      (is (= {:op :discard :id 0} (last (ops s)))))))

(deftest a-file-buffer
  (t/with-session [s :mode nil]
    (t/send! s {:type :opened :path "/tmp/a.txt" :text "one\ntwo\n"})
    (is (= [] (ops s)) "opened, not changed")
    (t/send! s {:type :text :text "i"})
    (t/type! s "X")
    (let [[base :as kept] (ops s)]
      (is (= 1 (count kept)))
      (is (= {:path "/tmp/a.txt" :name nil :scratch? false :dir "/tmp" :mode nil}
             (assoc (:meta base) :name nil)))
      (is (= "Xone\ntwo\n" (:text base))))
    (testing "saving it leaves nothing to lose"
      (t/press! s sdl/K-ESCAPE)
      (t/command! s "write")
      (is (= {:op :discard :id 1} (last (ops s)))))
    (testing "editing it again starts over, a generation on"
      (t/send! s {:type :text :text "i"})
      (t/type! s "Y")
      (is (= {:op :base :id 1 :gen 2} (select-keys (last (ops s)) [:op :id :gen]))))
    (testing "closing a buffer discards it"
      (t/press! s sdl/K-ESCAPE)
      (t/command! s "close!")
      (is (= {:op :discard :id 1} (last (ops s)))))))

(deftest a-failing-host
  (t/with-session [s]
    (swap! s assoc :journal-error "disk full")
    (t/type! s "a")
    (is (= [] (ops s)))
    (is (= "Can't keep recovery files: disk full" (:message (t/app s))))
    (testing "it is kept anew once the host can"
      (swap! s assoc :journal-error nil)
      (t/type! s "b")
      (is (= [{:op :base :id 0 :gen 2 :text "ab" :insets [] :caret 2
               :meta {:path nil :name "scratch" :scratch? true :dir nil :mode nil}}]
             (ops s))))))

(deftest a-long-log-is-compacted
  (t/with-session [s]
    (with-redefs [journal/compact-chars 300]
      (t/type! s "a")
      (t/send! s {:type :text :text (apply str (repeat 500 "x"))})
      (let [kept (ops s)]
        (is (= [:base :base] (map :op kept)) "a new base in place of the log")
        (is (= [1 2] (map :gen kept)))
        (is (= (str "a" (apply str (repeat 500 "x"))) (:text (last kept))))))))
