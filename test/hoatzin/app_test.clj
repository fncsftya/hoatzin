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

;; ---------------------------------------------------------------- selection

(def shift sdl/KMOD-SHIFT)

(defn- point-of
  "Render-pixel coordinates of document position `pos`, mid-line."
  [s pos]
  (let [a  (t/app s)
        L  (:layout a)
        m  (* (:density @s) (:margin a))
        lh (layout/line-height L)
        [x k] (layout/caret L pos)]
    [(+ m x) (+ m (* k lh) (/ lh 2.0) (- (:scroll a)))]))

(deftest shift-arrows-select
  (with-session [s]
    (t/type! s "hoatzin")
    (dotimes [_ 3] (t/press! s sdl/K-LEFT shift))
    (is (= "zin" (t/selected s)))
    (is (= 4 (t/caret s)) "the caret moves with the selection's free end")
    (t/press! s sdl/K-RIGHT shift)
    (is (= "in" (t/selected s)) "and can come back")
    (dotimes [_ 2] (t/press! s sdl/K-RIGHT shift))
    (is (nil? (t/selected s)) "back to where it started selects nothing")
    (t/press! s sdl/K-LEFT (bit-or cmd shift))
    (is (= "hoatzin" (t/selected s)) "cmd-shift-left selects to the line start")))

(deftest shift-vertical-selects-lines
  (with-session [s]
    (t/type! s "first\nsecond\nthird")
    (t/press! s sdl/K-UP shift)
    (is (= 1 (caret-line s)))
    (is (str/ends-with? (t/selected s) "\nthird"))
    (is (not (str/includes? (t/selected s) "second")) "part of the line above")
    (t/press! s sdl/K-UP shift)
    (is (= 0 (caret-line s)))
    (is (str/ends-with? (t/selected s) "\nsecond\nthird"))
    (t/press! s sdl/K-UP (bit-or cmd shift))
    (is (= "first\nsecond\nthird" (t/selected s)))
    (t/press! s sdl/K-DOWN (bit-or cmd shift))
    (is (nil? (t/selected s)))))

(deftest arrows-collapse-the-selection
  (with-session [s]
    (t/type! s "one two three")
    (t/press! s sdl/K-LEFT cmd)
    (dotimes [_ 4] (t/press! s sdl/K-RIGHT))
    (dotimes [_ 3] (t/press! s sdl/K-RIGHT shift))   ; "two"
    (t/press! s sdl/K-LEFT)
    (is (= {:text "one two three" :caret 4} (t/doc s)) "left goes to the start")
    (dotimes [_ 3] (t/press! s sdl/K-RIGHT shift))
    (t/press! s sdl/K-LEFT shift)
    (t/press! s sdl/K-LEFT shift)
    (t/press! s sdl/K-RIGHT)
    (is (= {:text "one two three" :caret 5} (t/doc s))
        "right goes to the end, wherever the caret was")
    (t/press! s sdl/K-RIGHT shift)
    (t/press! s sdl/K-RIGHT shift)
    (t/press! s sdl/K-UP)
    (is (= 0 (t/caret s)) "up from the first line goes to the start")
    (is (nil? (t/selected s)))))

(deftest editing-replaces-the-selection
  (with-session [s]
    (t/type! s "a big bird")
    (dotimes [_ 5] (t/press! s sdl/K-LEFT))
    (dotimes [_ 3] (t/press! s sdl/K-LEFT shift))   ; "big"
    (t/type! s "smelly")
    (is (= {:text "a smelly bird" :caret 8} (t/doc s)) "typing replaces it")
    (dotimes [_ 7] (t/press! s sdl/K-LEFT shift))
    (t/press! s sdl/K-BACKSPACE)
    (is (= {:text "a bird" :caret 1} (t/doc s)) "backspace deletes just the selection")
    (t/press! s sdl/K-RIGHT shift)
    (t/press! s sdl/K-DELETE)
    (is (= {:text "abird" :caret 1} (t/doc s)) "as does delete")
    (t/press! s sdl/K-RIGHT (bit-or cmd shift))
    (t/press! s sdl/K-BACKSPACE cmd)
    (is (= {:text "a" :caret 1} (t/doc s)) "cmd-backspace too, not to the line start")
    (t/press! s sdl/K-LEFT shift)
    (t/press! s sdl/K-RETURN)
    (is (= {:text "\n" :caret 1} (t/doc s)) "return replaces it")))

(deftest select-all
  (with-session [s]
    (t/type! s "one\ntwo")
    (t/press! s sdl/K-A cmd)
    (is (= "one\ntwo" (t/selected s)))
    (t/press! s sdl/K-A)
    (is (= "one\ntwo" (t/selected s)) "plain a is left to text input")))

(deftest copy-cut-and-paste
  (with-session [s]
    (t/type! s "stinkbird")
    (dotimes [_ 4] (t/press! s sdl/K-LEFT shift))
    (t/press! s sdl/K-C cmd)
    (is (= "bird" (:clipboard @s)))
    (is (= "bird" (t/selected s)) "copying keeps the selection")
    (t/press! s sdl/K-LEFT cmd)
    (dotimes [_ 5] (t/press! s sdl/K-RIGHT shift))
    (t/press! s sdl/K-X cmd)
    (is (= "stink" (:clipboard @s)))
    (is (= {:text "bird" :caret 0} (t/doc s)))
    (t/press! s sdl/K-RIGHT (bit-or cmd shift))
    (t/press! s sdl/K-V cmd)
    (is (= {:text "stink" :caret 5} (t/doc s)) "paste replaces the selection")
    (t/set-clipboard! s "unchanged")
    (t/press! s sdl/K-C cmd)
    (t/press! s sdl/K-X cmd)
    (is (= "unchanged" (:clipboard @s)) "with nothing selected, copy and cut do nothing")
    (is (= "stink" (t/text s)))))

(deftest drag-selects
  (with-session [s]
    (t/type! s "first line\nsecond line")
    (t/drag! s (point-of s 2) (point-of s 8) (point-of s 15))
    (is (= "rst line\nseco" (t/selected s)))
    (is (= 15 (t/caret s)))
    (t/send! s {:type :drag :x 0.0 :y 0.0})
    (is (= "rst line\nseco" (t/selected s)) "moving after the release selects nothing more")
    (t/drag! s (point-of s 15) (point-of s 2))
    (is (= "rst line\nseco" (t/selected s)) "dragging backwards works too")
    (is (= 2 (t/caret s)))
    (apply t/click! s (point-of s 5))
    (is (nil? (t/selected s)) "a click clears it")))

(deftest shift-click-extends
  (with-session [s]
    (t/type! s "first line\nsecond line")
    (apply t/click! s (point-of s 3))
    (t/click! s (first (point-of s 14)) (second (point-of s 14)) :mod shift)
    (is (= "st line\nsec" (t/selected s)))
    (t/click! s (first (point-of s 1)) (second (point-of s 1)) :mod shift)
    (is (= "ir" (t/selected s)) "from the same anchor")))

(deftest dragging-past-the-bottom-scrolls
  (with-session [s]
    (t/set-clipboard! s (str/join "\n" (map #(str "Line " %) (range 1 41))))
    (t/press! s sdl/K-V cmd)
    (t/press! s sdl/K-UP cmd)
    (let [[_ h] (:size (t/app s))]
      (t/drag! s [10.0 10.0] [10.0 (double (+ h 100))]))
    (is (pos? (:scroll (t/app s))))
    (is (caret-visible-on-screen? s))
    (is (str/starts-with? (t/selected s) "Line 1\nLine 2\n"))))

(deftest composing-replaces-the-selection
  (with-session [s]
    (t/type! s "cafe")
    (t/press! s sdl/K-LEFT shift)
    (t/dead-key! s "´" "é")
    (is (= {:text "café" :caret 4} (t/doc s)))))

(deftest a-selection-replaces-the-caret
  (with-session [s]
    (t/type! s "ab")
    (t/press! s sdl/K-LEFT shift)
    (is (not (app/caret-visible? (t/app s) 0)))
    (is (= -1 (app/ms-until-blink (t/app s) 0)) "no blinking to wake up for")
    (t/press! s sdl/K-RIGHT)
    (is (app/caret-visible? (t/app s) (:now @s)))))

;; ---------------------------------------------------------------- wrap points

(def long-words
  (apply str (repeat 8 "dfffasaghdignisaopgndispagndipagnipasgnid ")))

(defn- line-y
  "Render-pixel y of the middle of visual line `k`."
  [s k]
  (let [a (t/app s)
        lh (layout/line-height (:layout a))]
    (+ (* (:density @s) (:margin a)) (* k lh) (/ lh 2.0) (- (:scroll a)))))

(defn- caret-x [s] (first (app/caret-rect (t/app s))))
(defn- caret-row [s]
  (let [a (t/app s)
        [_ y] (app/caret-rect a)
        {:keys [line-height caret-top]} (get-in a [:layout :metrics])]
    (quot (- (+ y (:scroll a)) caret-top (long (* (:density @s) (:margin a)))) line-height)))

(defn- x-of [s pos] (+ (* (:density @s) (:margin (t/app s)))
                       (long (Math/floor (first (layout/caret (:layout (t/app s)) pos))))))

(deftest clicking-past-a-wrapped-line-goes-after-its-space
  (with-session [s :width 800 :height 600]   ; the default window
    (t/type! s long-words)
    (let [L (:layout (t/app s))]
      (is (< 2 (layout/line-count L)) "the text wraps")
      (doseq [k (range (dec (layout/line-count L)))
              :let [wrap (layout/line-start L (inc k))]]
        (t/click! s 5000.0 (line-y s k))
        (is (= wrap (t/caret s)) (str "line " k ": the caret is after the space"))
        (is (= " " (subs (t/text s) (dec wrap) wrap)))
        (is (= k (caret-row s)) "and drawn on the clicked line")
        (is (> (caret-x s) (x-of s (dec wrap))) "to the right of the space's start")))))

(deftest typing-at-the-end-of-a-wrapped-line
  (with-session [s :width 800 :height 600]   ; the default window
    (t/type! s long-words)
    (let [wrap (layout/line-start (:layout (t/app s)) 1)]
      (t/click! s 5000.0 (line-y s 0))
      (t/type! s "x")
      (is (= {:text (str (subs long-words 0 wrap) "x" (subs long-words wrap)) :caret (inc wrap)}
             (t/doc s))))))

(deftest keys-at-the-end-of-a-wrapped-line
  (with-session [s :width 800 :height 600]   ; the default window
    (t/type! s long-words)
    (t/press! s sdl/K-UP cmd)
    (let [wrap (layout/line-start (:layout (t/app s)) 1)]
      (t/press! s sdl/K-END)
      (is (= [wrap 0] [(t/caret s) (caret-row s)]) "end stays on the line, after its space")
      (t/press! s sdl/K-END)
      (is (= [wrap 0] [(t/caret s) (caret-row s)]) "and again is still there")
      (t/press! s sdl/K-HOME)
      (is (= 0 (t/caret s)) "home goes back to the start of the same line")
      (t/press! s sdl/K-RIGHT cmd)
      (t/press! s sdl/K-DOWN)
      (is (= 1 (caret-row s)) "down from there goes to the next line")
      (t/press! s sdl/K-UP)
      (is (= [wrap 0] [(t/caret s) (caret-row s)]) "and up comes back")
      (t/press! s sdl/K-RIGHT)
      (is (= [(inc wrap) 1] [(t/caret s) (caret-row s)]))
      (t/press! s sdl/K-LEFT)
      (is (= [wrap 1] [(t/caret s) (caret-row s)])
          "arriving at the wrap point from the right puts it on the next line")
      (t/press! s sdl/K-LEFT cmd)
      (is (= wrap (t/caret s)) "whose start it is")
      (t/press! s sdl/K-RIGHT (bit-or cmd shift))
      (is (= (subs long-words wrap (layout/line-start (:layout (t/app s)) 2)) (t/selected s))
          "cmd-shift-right selects the whole line, with its space"))))

;; ---------------------------------------------------------------- double click

(defn- double-click-at! [s pos] (apply t/double-click! s (point-of s pos)))

(deftest double-click-selects-a-word
  (with-session [s]
    (t/type! s "the stinky   hoatzin\n\nbird")
    (double-click-at! s 6)
    (is (= "stinky" (t/selected s)))
    (double-click-at! s 4)
    (is (= "stinky" (t/selected s)) "clicked at its very start")
    (double-click-at! s 12)
    (is (= "   " (t/selected s)) "whitespace selects the whitespace")
    (let [[_ y] (point-of s 0)]
      (t/double-click! s 5000.0 y))
    (is (= "hoatzin" (t/selected s)) "past the end of a line, the last word")
    (t/double-click! s 5000.0 (line-y s 1))
    (is (= {:text "the stinky   hoatzin\n\nbird" :caret 21} (t/doc s))
        "on a blank line, just the caret")
    (double-click-at! s 23)
    (is (= "bird" (t/selected s)))))

(deftest double-click-on-a-trailing-space
  (with-session [s :width 800 :height 600]   ; the default window
    (t/type! s long-words)
    (t/double-click! s 5000.0 (line-y s 0))
    (is (= " " (t/selected s)) "past a wrapped line's end is its space")))

(deftest drag-after-a-double-click-selects-words
  (with-session [s]
    (t/type! s "one two three four")
    (let [[x y] (point-of s 5)]
      (t/click! s x y)
      (t/send! s {:type :click :x x :y y :clicks 2}
               {:type :drag :x (+ x 1.0) :y y})
      (is (= "two" (t/selected s)) "a jitter keeps the word")
      (apply t/send! s (map (fn [[x y]] {:type :drag :x x :y y}) [(point-of s 10)]))
      (is (= "two three" (t/selected s)) "forwards, whole words")
      (apply t/send! s (map (fn [[x y]] {:type :drag :x x :y y}) [(point-of s 1)]))
      (is (= "one two" (t/selected s)) "backwards, keeping the first word")
      (t/send! s {:type :release})
      (t/send! s {:type :drag :x 5000.0 :y y})
      (is (= "one two" (t/selected s)) "until the button comes up"))))
