(ns hoatzin.app-test
  "Editor behaviour, driven headlessly by synthetic events. These assert on
  state; render_test checks pixels."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hoatzin.app :as app]
            [hoatzin.layout :as layout]
            [hoatzin.sdl :as sdl]
            [hoatzin.test-support :as t :refer [with-session]]))

(def cmd sdl/KMOD-GUI)

(defn- caret-line [s] (second (layout/caret (:layout (t/app s)) (t/caret s))))

(defn- caret-visible-on-screen? [s]
  (let [a (t/app s)
        [_ y _ h] (app/caret-rect a)]
    (and (>= y 0) (<= (+ y h) (second (:size a))))))

(deftest starts-empty
  (with-session [s]
    (is (= {:text "" :caret 0} (t/doc s)))
    (is (= 1 (layout/line-count (:layout (t/app s)))))))

(deftest typing
  (with-session [s]
    (t/type! s "Hello")
    (is (= {:text "Hello" :caret 5} (t/doc s)))
    (t/press! s sdl/K-LEFT)
    (t/press! s sdl/K-LEFT)
    (t/type! s "--")
    (is (= {:text "Hel--lo" :caret 5} (t/doc s)))))

(deftest backspace-and-delete
  (with-session [s]
    (t/type! s "a👍🏽b")
    (t/press! s sdl/K-LEFT)
    (t/press! s sdl/K-BACKSPACE)
    (is (= {:text "ab" :caret 1} (t/doc s)) "one press removes the whole emoji")
    (t/press! s sdl/K-DELETE)
    (is (= {:text "a" :caret 1} (t/doc s)))
    (t/press! s sdl/K-DELETE)
    (is (= "a" (t/text s)) "delete at the end does nothing")
    (t/press! s sdl/K-BACKSPACE)
    (t/press! s sdl/K-BACKSPACE)
    (is (= {:text "" :caret 0} (t/doc s)) "backspace at the start does nothing")))

(deftest return-splits-paragraphs
  (with-session [s]
    (t/type! s "onetwo")
    (dotimes [_ 3] (t/press! s sdl/K-LEFT))
    (t/press! s sdl/K-RETURN)
    (is (= {:text "one\ntwo" :caret 4} (t/doc s)))
    (is (= 1 (caret-line s)))
    (t/press! s sdl/K-BACKSPACE)
    (is (= {:text "onetwo" :caret 3} (t/doc s)) "backspace joins them again")))

(deftest arrows-stop-at-the-ends
  (with-session [s]
    (t/type! s "ab")
    (t/press! s sdl/K-RIGHT)
    (is (= 2 (t/caret s)))
    (dotimes [_ 5] (t/press! s sdl/K-LEFT))
    (is (= 0 (t/caret s)))))

(deftest vertical-movement-keeps-its-column
  (with-session [s]
    (t/type! s "a long first line\nab\nanother long line")
    (t/press! s sdl/K-LEFT cmd)
    (dotimes [_ 9] (t/press! s sdl/K-RIGHT))
    (let [start (t/caret s)]                ; "another l|ong line"
      (t/press! s sdl/K-UP)
      (is (= 1 (caret-line s)))
      (is (= "a long first line\nab" (subs (t/text s) 0 (t/caret s)))
          "a short line clamps the caret to its end")
      (t/press! s sdl/K-UP)
      (is (= 0 (caret-line s)))
      (t/press! s sdl/K-DOWN)
      (t/press! s sdl/K-DOWN)
      (is (= start (t/caret s)) "the column comes back after the short line"))
    (t/press! s sdl/K-DOWN)
    (is (= (count (t/text s)) (t/caret s)) "down from the last line goes to the end")
    (t/press! s sdl/K-UP sdl/KMOD-GUI)
    (is (= 0 (t/caret s)) "cmd-up goes to the start")))

(deftest line-start-and-end
  (with-session [s]
    (t/type! s "first\nsecond")
    (t/press! s sdl/K-LEFT cmd)
    (is (= 6 (t/caret s)))
    (t/press! s sdl/K-RIGHT cmd)
    (is (= 12 (t/caret s)))
    (t/press! s sdl/K-HOME)
    (is (= 6 (t/caret s)))
    (t/press! s sdl/K-END)
    (is (= 12 (t/caret s)))
    (t/press! s sdl/K-BACKSPACE cmd)
    (is (= {:text "first\n" :caret 6} (t/doc s)) "cmd-backspace deletes to line start")))

(deftest paste
  (with-session [s]
    (t/set-clipboard! s "one\r\ntwo\rthree")
    (t/press! s sdl/K-V cmd)
    (is (= {:text "one\ntwo\nthree" :caret 13} (t/doc s)) "line endings are normalized")
    (t/press! s sdl/K-V)
    (is (= 13 (t/caret s)) "plain v is left to text input"))
  (with-session [s]
    (t/press! s sdl/K-V cmd)
    (is (= "" (t/text s)) "an empty clipboard pastes nothing")))

(deftest click-places-the-caret
  (with-session [s :density 2.0]
    (t/type! s "first\nsecond")
    (let [{:keys [margin]} (t/app s)
          m  (* 2 margin)
          lh (layout/line-height (:layout (t/app s)))]
      (t/click! s (+ m 1.0) (+ m lh (/ lh 2.0)))
      (is (= 6 (t/caret s)) "the start of the second line")
      (t/click! s 5000.0 (+ m 1.0))
      (is (= 5 (t/caret s)) "past the end of the first line")
      (t/click! s (+ m 1.0) 5000.0)
      (is (= 1 (caret-line s)) "below the text picks the last line"))))

(deftest scrolling-follows-the-caret
  (with-session [s]
    (t/set-clipboard! s (str/join "\n" (map #(str "Line " %) (range 1 41))))
    (t/press! s sdl/K-V cmd)
    (is (pos? (:scroll (t/app s))) "pasting past the bottom scrolls")
    (is (caret-visible-on-screen? s))
    (t/press! s sdl/K-UP cmd)
    (is (zero? (:scroll (t/app s))) "and the start scrolls back")
    (t/send! s {:type :wheel :dy -2})
    (is (pos? (:scroll (t/app s))) "the wheel scrolls")
    (t/send! s {:type :wheel :dy 1000})
    (is (zero? (:scroll (t/app s))) "but not above the top")))

(deftest resizing-rewraps-and-keeps-the-caret-in-view
  (with-session [s :width 400 :height 200]
    (t/type! s (str/join " " (repeat 40 "hoatzin")))
    (let [lines (layout/line-count (:layout (t/app s)))]
      (t/resize! s 200 200)
      (is (> (layout/line-count (:layout (t/app s))) lines))
      (is (caret-visible-on-screen? s)))))

(defn- shown-text
  "The text the layout is showing, line by line, joined."
  [s]
  (let [L (:layout (t/app s))]
    (str/join "|" (map #(:text (layout/visual-line L %)) (range (layout/line-count L))))))

(deftest dead-key-composition
  (with-session [s]
    (t/type! s "caf")
    (t/compose! s "´" 1)
    (testing "while composing"
      (is (= {:text "caf" :caret 3} (t/doc s)) "the document is untouched")
      (is (= "caf´" (shown-text s)) "the accent is shown at the caret")
      (let [a (t/app s)
            [after-accent _] (layout/caret (:layout a) 4)]
        (is (= (+ (* 2 (:margin a)) (long (Math/floor after-accent)))
               (first (app/caret-rect a)))
            "the caret sits after the accent")))
    (t/send! s {:type :composition :text "" :cursor 0} {:type :text :text "é"})
    (testing "after the next key"
      (is (= {:text "café" :caret 4} (t/doc s)))
      (is (= "café" (shown-text s)))
      (is (nil? (:composition (t/app s)))))))

(deftest composition-mid-text
  (with-session [s]
    (t/type! s "pin ata")
    (dotimes [_ 3] (t/press! s sdl/K-LEFT))
    (t/compose! s "˜")
    (is (= "pin ˜ata" (shown-text s)))
    (t/dead-key! s "˜" "ñ")
    (is (= {:text "pin ñata" :caret 5} (t/doc s)))))

(deftest cancelled-composition-leaves-no-trace
  (with-session [s]
    (t/type! s "abc")
    (t/compose! s "¨")
    (t/compose! s "")
    (is (= {:text "abc" :caret 3} (t/doc s)))
    (is (= "abc" (shown-text s)))))

(deftest keys-and-clicks-belong-to-the-input-method-while-composing
  (with-session [s]
    (t/type! s "abc")
    (t/compose! s "´")
    (t/press! s sdl/K-LEFT)
    (t/press! s sdl/K-BACKSPACE)
    (t/click! s 0.0 0.0)
    (is (= {:text "abc" :caret 3} (t/doc s)))
    (is (= "abc´" (shown-text s)))))

(deftest a-dead-key-before-a-letter-it-cannot-combine-with
  ;; option-u then x: macOS commits the umlaut, then the x
  (with-session [s]
    (t/compose! s "¨" 1)
    (t/send! s {:type :composition :text "" :cursor 0} {:type :text :text "¨"} {:type :text :text "x"})
    (is (= {:text "¨x" :caret 2} (t/doc s)))))

(deftest caret-blinks
  (with-session [s]
    (let [visible? #(app/caret-visible? (t/app s) (:now @s))]
      (is (visible?))
      (is (= 530 (app/ms-until-blink (t/app s) 0)))
      (t/advance! s 530)
      (is (not (visible?)))
      (t/advance! s 530)
      (is (visible?))
      (t/advance! s 600)
      (is (not (visible?)))
      (t/type! s "x")
      (is (visible?) "typing shows the caret straight away")
      (t/send! s {:type :focus :focused? false})
      (is (not (visible?)) "hidden while unfocused")
      (is (= -1 (app/ms-until-blink (t/app s) (:now @s))) "and nothing to wake up for"))))

(deftest redraws-only-when-needed
  (with-session [s]
    (t/render! s)
    (is (not (app/needs-draw? (t/app s) 0)))
    (is (app/needs-draw? (t/app s) 530) "the caret toggling needs a frame")
    (t/type! s "a")
    (is (app/needs-draw? (t/app s) 0))))

(deftest unchanged-lines-keep-their-textures
  (with-session [s]
    (t/type! s "first paragraph\nsecond")
    (t/render! s)
    (let [tex #(get-in @(:textures (t/app s)) [:entries "first paragraph" :texture])
          before (tex)]
      (is before)
      (t/type! s " grows")
      (t/render! s)
      (is (= before (tex)) "editing another paragraph does not re-rasterize this one"))))
