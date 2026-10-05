(ns hoatzin.app-test
  "Editor behaviour, driven headlessly by synthetic events. These assert on
  state; render_test checks pixels."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hoatzin.app :as app]
            [hoatzin.app.buffers :as buffers]
            [hoatzin.app.command :as command]
            [hoatzin.app.dropdown :as dropdown]
            [hoatzin.app.geometry :as geo]
            [hoatzin.lib.layout :as layout]
            [hoatzin.lib.sdl :as sdl]
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
    (is (zero? (:scroll (t/app s))) "the wheel starts a glide")
    (is (app/ms-until-wake (t/app s) (:now @s)) "which wakes the loop")
    (t/advance! s 20)
    (t/send! s {:type :tick})
    (let [part (:scroll (t/app s))]
      (is (pos? part) "that moves the text")
      (t/advance! s 2000)
      (t/send! s {:type :tick})
      (is (> (:scroll (t/app s)) part) "on to where the wheel sent it")
      (is (= -1 (app/ms-until-wake (t/app s) (:now @s))) "and stops"))
    (t/send! s {:type :wheel :dy 1000})
    (t/advance! s 2000)
    (t/send! s {:type :tick})
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
      (is (= 530 (app/ms-until-wake (t/app s) 0)))
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
      (is (= -1 (app/ms-until-wake (t/app s) (:now @s))) "and nothing to wake up for"))))

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
    (t/press! s sdl/K-HOME)
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
    (is (= -1 (app/ms-until-wake (t/app s) 0)) "no blinking to wake up for")
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
      (t/press! s sdl/K-END)
      (t/press! s sdl/K-DOWN)
      (is (= 1 (caret-row s)) "down from there goes to the next line")
      (t/press! s sdl/K-UP)
      (is (= [wrap 0] [(t/caret s) (caret-row s)]) "and up comes back")
      (t/press! s sdl/K-RIGHT)
      (is (= [(inc wrap) 1] [(t/caret s) (caret-row s)]))
      (t/press! s sdl/K-LEFT)
      (is (= [wrap 1] [(t/caret s) (caret-row s)])
          "arriving at the wrap point from the right puts it on the next line")
      (t/press! s sdl/K-HOME)
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

;; ---------------------------------------------------------------- scroll bar

(def forty-lines (str/join "\n" (map #(str "Line " %) (range 1 41))))

(defn- long-session!
  "Paste forty lines and go back to the top."
  [s]
  (t/set-clipboard! s forty-lines)
  (t/press! s sdl/K-V cmd)
  (t/press! s sdl/K-UP cmd))

(defn- max-scroll [s] (:max-scroll (app/scrollbar (t/app s))))
(defn- bar-x [s] (let [{:keys [x w]} (app/scrollbar (t/app s))] (+ x (/ w 2.0))))
(defn- thumb-mid [s] (let [{:keys [thumb-y thumb-h]} (app/scrollbar (t/app s))]
                       (+ thumb-y (/ thumb-h 2.0))))

(deftest scrollbar-only-when-the-text-overflows
  (with-session [s]
    (t/type! s "short")
    (is (nil? (app/scrollbar (t/app s))))
    (long-session! s)
    (let [{:keys [x w top height thumb-y thumb-h]} (app/scrollbar (t/app s))
          [width h] (:size (t/app s))]
      (is (= width (+ x w)) "at the right edge")
      (is (= top thumb-y) "the thumb starts at the top")
      (is (< 0 thumb-h height) "and is shorter than the track")
      (is (< (+ top height) h)))))

(deftest the-thumb-follows-the-scroll
  (with-session [s]
    (long-session! s)
    (t/press! s sdl/K-DOWN cmd)
    (let [{:keys [top height thumb-y thumb-h]} (app/scrollbar (t/app s))]
      (is (= (max-scroll s) (:scroll (t/app s))))
      (is (= (+ top height) (+ thumb-y thumb-h)) "at the end, the thumb is at the bottom"))))

(deftest dragging-the-thumb-scrolls
  (with-session [s]
    (long-session! s)
    (let [doc (t/doc s)
          [_ h] (:size (t/app s))
          y (thumb-mid s)]
      (t/send! s {:type :click :x (bar-x s) :y y})
      (is (= :arrow (app/pointer (t/app s))) "the arrow while holding the thumb")
      (t/send! s {:type :drag :x 0.0 :y (+ y 40.0)})
      (let [mid (:scroll (t/app s))]
        (is (< 0 mid (max-scroll s)) "part way")
        (t/send! s {:type :drag :x 0.0 :y (+ y 80.0)})
        (is (< mid (:scroll (t/app s))) "and further"))
      (t/send! s {:type :drag :x 0.0 :y (double (* 2 h))})
      (is (= (max-scroll s) (:scroll (t/app s))) "no further than the end")
      (t/send! s {:type :drag :x 0.0 :y -1000.0})
      (is (zero? (:scroll (t/app s))) "nor above the top")
      (t/send! s {:type :release})
      (is (= doc (t/doc s)) "the caret and text are untouched")
      (t/send! s {:type :drag :x 0.0 :y (double h)})
      (is (zero? (:scroll (t/app s))) "let go, the thumb stays put"))))

(deftest clicking-the-track-pages
  (with-session [s]
    (long-session! s)
    (let [[_ h] (:size (t/app s))
          lh (layout/line-height (:layout (t/app s)))]
      (t/click! s (bar-x s) (- h 4.0))
      (let [page (:scroll (t/app s))]
        (is (pos? page) "below the thumb pages down")
        (is (< page (second (:size (t/app s)))) "by less than a screen")
        (is (>= page lh))
        (t/click! s (bar-x s) (- h 4.0))
        (let [twice (:scroll (t/app s))]
          (is (= (min (* 2 page) (max-scroll s)) twice))
          (t/click! s (bar-x s) 4.0)
          (is (= (- twice page) (:scroll (t/app s))) "above it pages up"))
        (is (= 0 (t/caret s)) "the caret stays where it was")
        (is (nil? (t/selected s)))))))

(deftest the-scroll-bar-works-while-composing
  (with-session [s]
    (long-session! s)
    (t/compose! s "´")
    (t/click! s (bar-x s) (- (second (:size (t/app s))) 4.0))
    (is (pos? (:scroll (t/app s))))
    (is (some? (:composition (t/app s))))))

(deftest hovering-the-scroll-bar
  (with-session [s]
    (long-session! s)
    (t/render! s)
    (is (= :text (app/pointer (t/app s))))
    (t/send! s {:type :move :x (bar-x s) :y 40.0})
    (is (= :arrow (app/pointer (t/app s))) "an arrow over the bar")
    (is (app/needs-draw? (t/app s) 0) "which widens")
    (t/render! s)
    (t/send! s {:type :move :x (bar-x s) :y 60.0})
    (is (not (app/needs-draw? (t/app s) 0)) "moving along it changes nothing")
    (t/send! s {:type :move :x 40.0 :y 60.0})
    (is (= :text (app/pointer (t/app s))) "an I-beam over the text")
    (t/send! s {:type :move :x (bar-x s) :y 40.0} {:type :leave})
    (is (= :text (app/pointer (t/app s))) "and once the pointer leaves the window"))
  (with-session [s]
    (t/type! s "fits")
    (t/send! s {:type :move :x 795.0 :y 40.0})
    (is (= :text (app/pointer (t/app s))) "no bar, no arrow")))

(deftest a-drag-held-below-the-text-keeps-scrolling
  (with-session [s]
    (long-session! s)
    (let [[_ h] (:size (t/app s))
          below (double (- h 2))]
      (t/send! s {:type :click :x 60.0 :y 60.0} {:type :drag :x 60.0 :y below})
      (let [scroll (:scroll (t/app s))
            caret  (t/caret s)]
        (is (= 50 (app/ms-until-wake (t/app s) (:now @s))) "it asks to wake up soon")
        (t/advance! s 50)
        (t/send! s {:type :tick})
        (is (< scroll (:scroll (t/app s))) "and scrolls when it does")
        (is (< caret (t/caret s)) "extending the selection"))
      (dotimes [_ 40] (t/send! s {:type :tick}))
      (is (= (max-scroll s) (:scroll (t/app s))) "until the end")
      (is (= (dec (layout/line-count (:layout (t/app s)))) (caret-line s)) "the last line")
      (let [a (t/app s)]
        (t/send! s {:type :tick})
        (is (identical? a (t/app s)) "where ticks change nothing"))
      (t/send! s {:type :release})
      (is (= -1 (app/ms-until-wake (t/app s) (:now @s))) "let go, nothing to wake for"))))

(deftest a-drag-held-above-the-text-scrolls-up
  (with-session [s]
    (t/set-clipboard! s forty-lines)
    (t/press! s sdl/K-V cmd)
    (let [scroll (:scroll (t/app s))]
      (t/send! s {:type :click :x 60.0 :y 200.0} {:type :drag :x 60.0 :y 2.0})
      (dotimes [_ 3] (t/send! s {:type :tick}))
      (is (< (:scroll (t/app s)) scroll)))))

(deftest ticks-without-a-drag-do-nothing
  (with-session [s]
    (long-session! s)
    (let [a (t/app s)]
      (t/send! s {:type :tick})
      (is (= (dissoc a :layout) (dissoc (t/app s) :layout))))))

(deftest an-off-screen-caret-does-not-blink
  (with-session [s]
    (long-session! s)
    (is (= 530 (app/ms-until-wake (t/app s) 0)))
    (t/send! s {:type :wheel :dy -10})
    (t/advance! s 2000)
    (t/send! s {:type :tick})
    (is (= -1 (app/ms-until-wake (t/app s) 0)) "scrolled away, nothing to wake for")
    (t/render! s)
    (is (not (app/needs-draw? (t/app s) 530)) "nor to redraw")))

;; ---------------------------------------------------------------- modes

(defn- press-i!
  "The i key as SDL reports it: the key, then its text."
  [s]
  (t/send! s {:type :key :key 0x69 :mod 0} {:type :text :text "i"}))

(deftest starts-in-normal-mode
  (with-session [s :mode nil]
    (is (= :normal (:mode (t/app s))))))

(deftest normal-mode-leaves-the-text-alone
  (with-session [s]
    (t/type! s "one\ntwo")
    (t/press! s sdl/K-ESCAPE)
    (is (= :normal (:mode (t/app s))))
    (t/type! s "xyz")
    (t/press! s sdl/K-BACKSPACE)
    (t/press! s sdl/K-DELETE)
    (t/press! s sdl/K-RETURN)
    (t/compose! s "´" 1)
    (t/set-clipboard! s "pasted")
    (t/press! s sdl/K-V cmd)
    (is (= {:text "one\ntwo" :caret 7} (t/doc s)))
    (is (nil? (:composition (t/app s))))
    (t/press! s sdl/K-A cmd)
    (t/press! s sdl/K-X cmd)
    (is (= "one\ntwo" (t/text s)) "cut copies but does not delete")
    (t/press! s sdl/K-UP)
    (t/press! s sdl/K-LEFT)
    (is (= 0 (t/caret s)) "but the caret still moves")))

(deftest i-and-escape-switch-modes
  (with-session [s :mode :normal]
    (press-i! s)
    (is (= :insert (:mode (t/app s))))
    (is (= "" (t/text s)) "the i that switched modes is not typed")
    (press-i! s)
    (is (= "i" (t/text s)) "in insert mode, i is typed")
    (t/press! s sdl/K-ESCAPE)
    (is (= :normal (:mode (t/app s))))
    (t/press! s sdl/K-ESCAPE)
    (is (= :normal (:mode (t/app s))) "escape in normal mode stays there")))

(deftest the-caret-is-a-block-in-normal-mode
  (with-session [s]
    (t/type! s "wide")
    (t/press! s sdl/K-UP cmd)
    (let [bar (app/caret-rect (t/app s))]
      (t/press! s sdl/K-ESCAPE)
      (let [[x y w h] (app/caret-rect (t/app s))
            L (:layout (t/app s))]
        (is (= [(first bar) y h] [x (second bar) (nth bar 3)]))
        (is (= w (long (Math/ceil (first (layout/caret L 1))))) "as wide as the w")
        (t/press! s sdl/K-END)
        (is (pos? (nth (app/caret-rect (t/app s)) 2)) "and still there at the end")))))

(deftest switching-modes-redraws
  (with-session [s]
    (t/render! s)
    (t/press! s sdl/K-ESCAPE)
    (is (app/needs-draw? (t/app s) 0))))

;; ---------------------------------------------------------------- commands

(deftest colon-starts-a-command-line
  (with-session [s :mode :normal]
    (t/type! s ":ope")
    (is (= :command (:mode (t/app s))))
    (is (= "ope" (:command (t/app s))))
    (is (= "" (t/text s)) "the document is untouched")
    (t/press! s sdl/K-BACKSPACE)
    (is (= "op" (:command (t/app s))))
    (t/press! s sdl/K-LEFT)
    (is (= "op" (:command (t/app s))) "other keys do nothing")
    (t/press! s sdl/K-ESCAPE)
    (is (= :normal (:mode (t/app s))) "escape abandons it")
    (is (nil? (:command (t/app s))))
    (t/type! s ":")
    (t/press! s sdl/K-BACKSPACE)
    (is (= :normal (:mode (t/app s))) "as does backspacing past the colon")
    (is (zero? (:dialogs @s)) "and nothing ran")))

(deftest colon-is-typed-in-insert-mode
  (with-session [s]
    (t/type! s ":open")
    (is (= ":open" (t/text s)))))

(deftest open-shows-the-dialog
  (with-session [s :mode :normal]
    (t/command! s "open")
    (is (= 1 (:dialogs @s)))
    (is (= :normal (:mode (t/app s))))
    (t/command! s "  open ")
    (is (= 2 (:dialogs @s)) "surrounding spaces are ignored")))

(deftest an-unknown-command-says-so
  (with-session [s :mode :normal]
    (t/command! s "frobnicate")
    (is (= "Not an editor command: frobnicate" (:message (t/app s))))
    (t/press! s sdl/K-LEFT)
    (is (nil? (:message (t/app s))) "until the next keystroke")
    (t/command! s "")
    (is (nil? (:message (t/app s))) "an empty command does nothing")))

(defn- buffer-names
  "The buffers' names, in order."
  [s]
  (let [bs (buffers/listing (t/app s))]
    (mapv (buffers/names bs) (map :buffer-id bs))))

(deftest the-opened-file-is-visited-in-a-buffer-of-its-own
  (with-session [s]
    (is (= ["scratch"] (buffer-names s)) "a session starts with the scratch buffer")
    (t/type! s "notes")
    (t/send! s {:type :opened :path "/birds/hoatzin.txt" :text "one\r\ntwo\rthree\n"})
    (is (= {:text "one\ntwo\nthree\n" :caret 0} (t/doc s)) "with line endings normalized")
    (is (= "/birds/hoatzin.txt" (:path (t/app s))))
    (is (= "/birds" (:dir (t/app s))) "its directory is the working directory")
    (is (= "\"hoatzin.txt\" 3 lines" (:message (t/app s))))
    (is (zero? (:scroll (t/app s))))
    (is (= ["scratch" "hoatzin.txt"] (buffer-names s)))
    (is (= "notes" (str (:text (:doc (first (buffers/listing (t/app s))))))) "the scratch buffer keeps its text")))

(deftest a-file-that-cannot-be-read-leaves-the-buffer-alone
  (with-session [s]
    (t/type! s "keep me")
    (t/send! s {:type :opened :path "/birds/secret.txt" :error "Permission denied"})
    (is (= "keep me" (t/text s)))
    (is (= "Can't open secret.txt: Permission denied" (:message (t/app s))))))

(deftest the-caret-is-on-the-command-line
  (with-session [s :mode :normal]
    (let [in-text (app/caret-rect (t/app s))]
      (t/type! s ":open")
      (let [[x y] (app/caret-rect (t/app s))]
        (is (> y (second in-text)) "below the text, in the status bar")
        (is (> x (first in-text)) "after the command")
        (is (app/caret-visible? (t/app s) (:now @s)))))))

(deftest a-prefix-runs-the-command-it-begins
  (with-session [s :mode :normal]
    (t/command! s "o")
    (is (= 1 (:dialogs @s)) ":o is :open")
    (t/command! s "op")
    (is (= 2 (:dialogs @s)))
    (t/command! s "sa")
    (is (= [nil] (:save-dialogs @s)) ":sa is :save")
    (t/command! s "s")
    (is (= "Ambiguous command: s (save, settings)" (:message (t/app s))))
    (t/command! s "opener")
    (is (= "Not an editor command: opener" (:message (t/app s))) "more than the name is not a prefix")))

(deftest tab-completes-the-command
  (with-session [s :mode :normal]
    (t/type! s ":w")
    (t/press! s sdl/K-TAB)
    (is (= "write" (:command (t/app s))))
    (t/press! s sdl/K-TAB)
    (is (= "write" (:command (t/app s))) "a complete name stays as it is")
    (t/press! s sdl/K-ESCAPE)
    (t/type! s ":x")
    (t/press! s sdl/K-TAB)
    (is (= "x" (:command (t/app s))) "nothing to complete")
    (t/press! s sdl/K-ESCAPE)
    (t/type! s ":")
    (t/press! s sdl/K-TAB)
    (is (= "" (:command (t/app s))) "every command begins with nothing")
    (is (= "" (t/text s)) "the document is untouched")))

(deftest write-saves-the-file
  (with-session [s]
    (t/send! s {:type :opened :path "/birds/hoatzin.txt" :text "one\n"})
    (t/type! s "zero ")
    (t/press! s sdl/K-ESCAPE)
    (t/command! s "w")
    (is (= {"/birds/hoatzin.txt" "zero one\n"} (:files @s)))
    (is (= "\"hoatzin.txt\" 1 lines written" (:message (t/app s))))
    (is (not (:modified? (t/app s))))))

(deftest q-quits-the-editor
  (with-session [s :mode :normal]
    (is (not (:quit? (t/app s))))
    (t/command! s "q")
    (is (:quit? (t/app s)))))

(deftest q-refuses-to-quit-with-unsaved-changes
  (with-session [s :mode :normal]
    (t/send! s {:type :opened :path "/birds/hoatzin.txt" :text "one\n"})
    (t/type! s "i")
    (t/type! s "zero")
    (t/press! s sdl/K-ESCAPE)
    (t/command! s "new")
    (t/command! s "q")
    (is (not (:quit? (t/app s))) "though the buffer shown has none")
    (is (= "Unsaved changes in \"hoatzin.txt\" (add ! to override)" (:message (t/app s))))
    (t/type! s "i")
    (t/type! s "two")
    (t/press! s sdl/K-ESCAPE)
    (t/command! s "q")
    (is (= "Unsaved changes in \"hoatzin.txt\", \"untitled\" (add ! to override)"
           (:message (t/app s))))
    (t/command! s "q!")
    (is (:quit? (t/app s)))))

(deftest the-scratch-buffer-does-not-stop-a-quit
  (with-session [s]
    (t/type! s "zero")
    (t/press! s sdl/K-ESCAPE)
    (is (:modified? (t/app s)))
    (t/command! s "q")
    (is (:quit? (t/app s))))
  (testing "unless it has been saved to a file"
    (with-session [s]
      (t/type! s "zero")
      (t/press! s sdl/K-ESCAPE)
      (t/command! s "w")
      (t/send! s {:type :save-chosen :path "/birds/zero.txt"})
      (t/type! s "i")
      (t/type! s "!")
      (t/press! s sdl/K-ESCAPE)
      (t/command! s "q")
      (is (not (:quit? (t/app s)))))))

(deftest write-without-a-path-asks-where
  (with-session [s]
    (t/type! s "new")
    (t/press! s sdl/K-ESCAPE)
    (t/command! s "write")
    (is (= [nil] (:save-dialogs @s)))
    (is (empty? (:files @s)) "nothing is written until a path is chosen")
    (t/send! s {:type :save-chosen :path "/birds/new.txt"})
    (is (= {"/birds/new.txt" "new"} (:files @s)))
    (is (= "/birds/new.txt" (:path (t/app s))))
    (t/command! s "write")
    (is (= [nil] (:save-dialogs @s)) "once it has one, it is written there")))

(deftest save-asks-where-starting-at-the-file
  (with-session [s :mode :normal]
    (t/send! s {:type :opened :path "/birds/hoatzin.txt" :text "one"})
    (t/command! s "save")
    (is (= ["/birds/hoatzin.txt"] (:save-dialogs @s)))
    (t/send! s {:type :save-chosen :path "/birds/copy.txt"})
    (is (= {"/birds/copy.txt" "one"} (:files @s)))
    (is (= "/birds/copy.txt" (:path (t/app s))) "the copy is the file now")))

(deftest a-failed-write-says-why
  (with-session [s]
    (t/send! s {:type :opened :path "/birds/hoatzin.txt" :text "one"})
    (t/type! s "zero ")
    (swap! s assoc :write-error "Permission denied")
    (t/press! s sdl/K-ESCAPE)
    (t/command! s "write")
    (is (= "Can't write hoatzin.txt: Permission denied" (:message (t/app s))))
    (is (:modified? (t/app s)) "the text still differs from the file")
    (t/send! s {:type :save-chosen :error "no dialogs here"})
    (is (= "Can't save: no dialogs here" (:message (t/app s))))))

(deftest modified-tracks-the-file
  (with-session [s]
    (is (not (:modified? (t/app s))) "an empty buffer is no change")
    (t/type! s "x")
    (is (:modified? (t/app s)))
    (t/press! s sdl/K-BACKSPACE)
    (is (not (:modified? (t/app s))) "undoing the change by hand undoes it")
    (t/send! s {:type :opened :path "/birds/hoatzin.txt" :text "one\ntwo\n"})
    (is (not (:modified? (t/app s))) "a file just opened")
    (t/type! s "a")
    (is (:modified? (t/app s)))
    (t/press! s sdl/K-BACKSPACE)
    (is (not (:modified? (t/app s))))
    (t/press! s sdl/K-RETURN)
    (is (:modified? (t/app s)) "a new line")
    (t/press! s sdl/K-BACKSPACE)
    (is (not (:modified? (t/app s))) "rejoined")))

(deftest modified-shows-and-redraws
  (with-session [s]
    (t/render! s)
    (t/type! s "x")
    (is (app/needs-draw? (t/app s) (:now @s)))
    (t/render! s)
    (t/press! s sdl/K-ESCAPE)
    (t/render! s)
    (t/command! s "save")
    (t/render! s)
    (t/send! s {:type :save-chosen :path "/birds/x.txt"})
    (is (app/needs-draw? (t/app s) (:now @s)) "saving clears [+]")))

;; ---------------------------------------------------------------- buffers

(defn- open! [s path text] (t/send! s {:type :opened :path path :text text}))

(deftest switching-buffers-keeps-each-ones-state
  (with-session [s :mode :normal]
    (open! s "/birds/a.txt" "alpha\nbeta\n")
    (t/press! s sdl/K-DOWN)
    (t/type! s "i")
    (t/type! s "x")
    (t/press! s sdl/K-ESCAPE)
    (open! s "/fish/b.txt" "bream")
    (is (= "bream" (t/text s)))
    (is (= "/fish" (:dir (t/app s))))
    (is (not (:modified? (t/app s))))
    (open! s "/birds/a.txt" "alpha\nbeta\n")
    (is (= "alpha\nxbeta\n" (t/text s)) "opening a file visited already switches to its buffer")
    (is (= 7 (t/caret s)) "with the caret where it was")
    (is (:modified? (t/app s)))
    (is (= "/birds" (:dir (t/app s))) "and its directory")
    (is (= ["scratch" "a.txt" "b.txt"] (buffer-names s)) "and no new buffer")
    (t/type! s "u")
    (is (= "alpha\nbeta\n" (t/text s)) "each buffer keeps its own undo")))

(deftest buffers-of-the-same-name
  (with-session [s :mode :normal]
    (open! s "/birds/notes.txt" "")
    (open! s "/fish/notes.txt" "")
    (t/command! s "new")
    (t/command! s "new")
    (is (= ["scratch" "notes.txt" "notes.txt<2>" "untitled" "untitled<2>"] (buffer-names s)))))

(deftest new-starts-an-empty-buffer
  (with-session [s :mode :normal :dir "/home"]
    (t/type! s "i")
    (t/type! s "scratch text")
    (t/press! s sdl/K-ESCAPE)
    (t/command! s "new")
    (is (= "" (t/text s)))
    (is (nil? (:path (t/app s))))
    (is (= "/home" (:dir (t/app s))) "in the directory of the buffer it was made from")
    (is (= ["scratch" "untitled"] (buffer-names s)))
    (t/command! s "w")
    (is (= ["/home"] (:save-dialogs @s)) "saving it starts in its directory")
    (t/send! s {:type :save-chosen :path "/birds/new.txt"})
    (is (= "/birds" (:dir (t/app s))) "and once saved, it is in its file's")
    (is (= ["scratch" "new.txt"] (buffer-names s)) "and named for it")))

(deftest close-closes-the-buffer
  (with-session [s :mode :normal]
    (open! s "/birds/a.txt" "alpha")
    (open! s "/birds/b.txt" "beta")
    (t/type! s "i")
    (t/type! s "x")
    (t/press! s sdl/K-ESCAPE)
    (t/command! s "close")
    (is (= "\"b.txt\" has unsaved changes (add ! to override)" (:message (t/app s))))
    (is (= ["scratch" "a.txt" "b.txt"] (buffer-names s)))
    (t/command! s "close!")
    (is (= ["scratch" "a.txt"] (buffer-names s)))
    (is (= "alpha" (t/text s)) "the buffer before it is shown")
    (is (= "Closed \"b.txt\"" (:message (t/app s))))
    (t/command! s "buffers")
    (t/press! s sdl/K-HOME)
    (t/press! s sdl/K-RETURN)
    (t/command! s "close")
    (is (= ["a.txt"] (buffer-names s)) "even the scratch buffer can be closed")
    (is (= "alpha" (t/text s)) "the first shows the one after it")
    (t/command! s "close")
    (is (= ["scratch"] (buffer-names s)) "closing the last leaves a new scratch buffer")
    (is (= "" (t/text s)))))

(deftest revert-reads-the-file-again
  (with-session [s :mode :normal]
    (swap! s assoc-in [:files "/birds/a.txt"] "alpha\n")
    (open! s "/birds/a.txt" "alpha\n")
    (t/press! s sdl/K-RIGHT)
    (t/press! s sdl/K-RIGHT)
    (t/type! s "i")
    (t/type! s "xyz")
    (t/press! s sdl/K-ESCAPE)
    (swap! s assoc-in [:files "/birds/a.txt"] "alpha\nbeta\r\n")
    (t/command! s "revert")
    (is (= "alpha\nbeta\n" (t/text s)) "as it is on disk now")
    (is (not (:modified? (t/app s))))
    (is (= 5 (t/caret s)) "the caret stays where it was")
    (is (= "Reverted \"a.txt\" 2 lines" (:message (t/app s))))
    (t/type! s "u")
    (is (= "alxyzpha\n" (t/text s)) "undo brings back what it replaced")
    (swap! s update :files dissoc "/birds/a.txt")
    (t/command! s "revert")
    (is (= "Can't revert a.txt: No such file" (:message (t/app s))))
    (is (= "alxyzpha\n" (t/text s)))))

(deftest revert-warns-without-a-file
  (with-session [s :mode :normal]
    (t/command! s "revert")
    (is (= "\"scratch\" has no file to revert to" (:message (t/app s))))
    (t/command! s "new")
    (t/command! s "revert")
    (is (= "\"untitled\" has no file to revert to" (:message (t/app s))))))

(deftest cd-chooses-the-buffers-directory
  (with-session [s :mode :normal :dir "/home"]
    (t/command! s "open")
    (is (= ["/home"] (:open-dialogs @s)) "the open dialog starts in the working directory")
    (t/command! s "cd")
    (is (= ["/home"] (:dir-dialogs @s)) "so does the directory dialog")
    (t/send! s {:type :dir-chosen :path "/birds"})
    (is (= "/birds" (:dir (t/app s))))
    (is (= "Directory /birds" (:message (t/app s))))
    (t/command! s "open")
    (is (= ["/home" "/birds"] (:open-dialogs @s)))
    (open! s "/fish/b.txt" "bream")
    (is (= "/fish" (:dir (t/app s))) "opening a file goes to its directory")
    (t/command! s "buffers")
    (t/press! s sdl/K-UP)
    (t/press! s sdl/K-RETURN)
    (is (= "/birds" (:dir (t/app s))) "and switching buffers to the buffer's")
    (t/send! s {:type :dir-chosen :error "no dialogs here"})
    (is (= "Can't change directory: no dialogs here" (:message (t/app s))))
    (is (= "/birds" (:dir (t/app s))))))

(deftest file-dialogs-start-where-the-last-one-chose
  (with-session [s :mode :normal :dir "/home"]
    (open! s "/fish/b.txt" "bream")
    (t/command! s "buffers")
    (t/press! s sdl/K-HOME)
    (t/press! s sdl/K-RETURN)
    (is (= "/home" (:dir (t/app s))) "back in the scratch buffer")
    (t/command! s "open")
    (is (= ["/fish"] (:open-dialogs @s)) "the open dialog starts where the last chose, for any buffer")
    (t/command! s "new")
    (t/command! s "save")
    (is (= ["/fish"] (:save-dialogs @s)) "so does the save dialog")
    (t/send! s {:type :save-chosen :path "/birds/new.txt"})
    (t/command! s "open")
    (is (= ["/fish" "/birds"] (:open-dialogs @s)) "saving chooses a directory too")
    (t/send! s {:type :opened :path "/trees/secret.txt" :error "Permission denied"})
    (t/command! s "open")
    (is (= "/trees" (last (:open-dialogs @s))) "even a file that couldn't be read")
    (t/command! s "save")
    (is (= "/birds/new.txt" (last (:save-dialogs @s))) "a buffer's file comes first, saving")
    (t/command! s "cd")
    (t/send! s {:type :dir-chosen :path "/rocks"})
    (t/command! s "open")
    (is (= "/rocks" (last (:open-dialogs @s))) "and so does :cd")))

(defn- buffer-rows
  "The buffers window's rows, placed: [index rect]."
  [s]
  (keep #(when-let [i (get-in % [:node :buffer-row])] [i (:rect %)]) (:float-places (t/app s))))

(defn- centre [[x y w h]] [(+ x (quot w 2)) (+ y (quot h 2))])

(deftest the-buffers-window
  (with-session [s :mode :normal]
    (open! s "/birds/a.txt" "alpha")
    (open! s "/birds/b.txt" "beta")
    (t/command! s "buffers")
    (is (= :buffers (:window (t/app s))))
    (is (= 2 (:buffers-active (t/app s))) "on the current buffer")
    (is (= [0 1 2] (map first (buffer-rows s))))
    (t/type! s "i")
    (is (= :normal (:mode (t/app s))) "the window takes the input")
    (t/press! s sdl/K-UP)
    (is (= 1 (:buffers-active (t/app s))))
    (t/press! s sdl/K-RETURN)
    (is (nil? (:window (t/app s))) "return closes it")
    (is (= "alpha" (t/text s)) "and switches to the buffer it is on")
    (t/command! s "buffers")
    (let [[x y] (centre (second (first (buffer-rows s))))]
      (t/send! s {:type :move :x x :y y})
      (is (= 0 (:buffers-active (t/app s))) "it is on the buffer under the pointer")
      (t/click! s x y))
    (is (nil? (:window (t/app s))) "a click closes it")
    (is (= "scratch" (buffers/buffer-name (t/app s))) "and switches to the buffer clicked")
    (t/command! s "buffers")
    (t/press! s sdl/K-ESCAPE)
    (is (nil? (:window (t/app s))) "escape closes it")
    (is (= "scratch" (buffers/buffer-name (t/app s))) "and stays put")))

(deftest the-buffers-window-scrolls
  (with-session [s :mode :normal :height 200]
    (dotimes [i 20] (open! s (str "/birds/" i ".txt") ""))
    (t/command! s "buffers")
    (let [rows   #(map first (buffer-rows s))
          [_ h]  (:size (t/app s))
          shown  (count (rows))]
      (is (< 1 shown 21))
      (is (= 20 (last (rows))) "scrolled to show the current buffer")
      (is (every? (fn [[_ [_ y _ rh]]] (<= (+ y rh) h)) (buffer-rows s)) "within the window")
      (t/press! s sdl/K-HOME)
      (is (= 0 (first (rows))))
      (t/send! s {:type :wheel :dy -1})
      (is (= 1 (first (rows))) "the wheel scrolls it")
      (t/press! s sdl/K-PAGEDOWN)
      (is (= shown (:buffers-active (t/app s))))
      (t/press! s sdl/K-END)
      (is (= 20 (:buffers-active (t/app s))))
      (is (= 20 (last (rows)))))))

;; ---------------------------------------------------------------- boxes

(defn- with-app! [s f & args]
  (swap! s update :app #(app/settle (apply f % args)))
  s)

(def ^:private panel
  {:kind :box :style {:height 50 :border 1 :padding 4 :direction :row :gap 4 :align :center}
   :children [{:kind :label :text "Notes"}
              {:kind :checkbox :id :done}]})

(deftest blocks-make-room-in-the-text
  (with-session [s]
    (t/type! s "one\ntwo\nthree")
    (let [lh (layout/line-height (:layout (t/app s)))
          caret-y #(second (app/caret-rect (t/app s)))
          y0 (caret-y)]
      (with-app! s app/add-block :b 1 panel)
      (is (= (+ y0 100) (caret-y)) "lines below the block move down by its height (2x density)")
      (t/press! s sdl/K-UP cmd)
      (is (= (- y0 (* 2 lh)) (caret-y)) "lines above it stay put")
      (testing "a click below the block lands on the line under it"
        (t/click! s 60.0 (+ y0 100 (quot lh 2)))
        (is (= 2 (caret-line s)))))))

(deftest blocks-move-with-the-text
  (with-session [s]
    (t/type! s "one\ntwo")
    (with-app! s app/add-block :b 5 panel)
    (is (= 1 (:line (first (:block-places (t/app s))))))
    (t/press! s sdl/K-UP cmd)
    (t/type! s "zero\n")
    (is (= 10 (get-in (t/app s) [:doc :marks :b])))
    (is (= 2 (:line (first (:block-places (t/app s))))) "still below \"two\"")
    (with-app! s app/remove-block :b)
    (is (empty? (:block-places (t/app s))))))

(deftest boxes-take-their-clicks
  (with-session [s]
    (t/type! s "one\ntwo")
    (with-app! s app/add-block :b 0 panel)
    (let [caret (t/caret s)
          {[x y w h] :rect} (->> (:block-places (t/app s)) first :placed
                                 (filter #(= :checkbox (get-in % [:node :kind]))) first)
          m (* 2 24)]
      (t/click! s (+ m x (quot w 2)) (+ m y (quot h 2)))
      (is (= caret (t/caret s)) "the caret stays put")
      (is (true? (get-in (t/app s) [:ui-values :done])) "the checkbox is ticked")
      (t/click! s (+ m x (quot w 2)) (+ m y (quot h 2)))
      (is (false? (get-in (t/app s) [:ui-values :done])))
      (t/send! s {:type :move :x (+ m x 1.0) :y (+ m y 1.0)})
      (is (= :arrow (app/pointer (t/app s))))))
  (testing "floats are over the text"
    (with-session [s]
      (t/type! s "one")
      (with-app! s app/set-floats [{:kind :box :style {:position :absolute :left 0 :top 0
                                                       :width 100 :height 100}}])
      (t/click! s 300.0 300.0)
      (is (= 3 (t/caret s)) "the click didn't reach the text"))))

(defn- hints
  "The command hints shown, as columns of names."
  [s]
  (when-let [{:keys [children]} (some #(when (= :row (get-in % [:node :style :direction])) (:node %))
                                      (:float-places (t/app s)))]
    (mapv #(mapv :text (:children %)) children)))

(deftest command-hints
  (with-session [s :mode nil :width 1000]
    (is (nil? (hints s)) "none outside the command line")
    (t/type! s ":")
    (is (= [["buffers"] ["cd"] ["close"] ["new"] ["open"] ["quit"] ["revert"] ["save"]
            ["settings"] ["write"]]
           (hints s))
        "every command, alphabetically, on one row while they fit")
    (t/type! s "sa")
    (is (= [["save"]] (hints s)) "only those the text begins")
    (t/type! s "ve!")
    (is (= [["save"]] (hints s)) "a trailing ! still names the command")
    (t/press! s sdl/K-BACKSPACE)
    (t/press! s sdl/K-BACKSPACE)
    (t/type! s "x")
    (is (nil? (hints s)) "none when the text begins no command")
    (t/press! s sdl/K-ESCAPE)
    (is (nil? (hints s)) "gone with the command line"))
  (testing "in two rows, down then across, when they don't fit on one"
    (with-session [s :mode nil :width 220]
      (t/type! s ":")
      (is (= [["buffers" "cd"] ["close" "new"]] (hints s)))))
  (testing "no more than two rows: the columns that don't fit are left out"
    (is (= [["a" "b"] ["c" "d"]] (#'command/hint-columns ["a" "b" "c" "d" "e" "f"] 10 5 25)))
    (is (= [["a"] ["b"] ["c"]] (#'command/hint-columns ["a" "b" "c"] 10 5 40)))
    (is (= [["a" "b"]] (#'command/hint-columns ["a" "b" "c"] 10 5 1)) "always one column")))

(defn- settings-box
  "The settings window's placed box, or nil when it is closed."
  [s]
  (some #(when (= "Settings" (:text (first (get-in % [:node :children])))) %)
        (:float-places (t/app s))))

(defn- settings-values
  "What the settings window's fields and dropdowns show, by :id."
  [s]
  (into {} (keep #(when (#{:field :dropdown} (get-in % [:node :kind]))
                    [(get-in % [:node :id]) (get-in % [:node :value])]))
        (:float-places (t/app s))))

(deftest settings-window
  (with-session [s :mode :normal]
    (is (nil? (settings-box s)) "closed to begin with")
    (t/command! s "settings")
    (let [{[x y w h] :rect} (settings-box s)
          m (* 2 (:margin (t/app s)))
          [sw sh] (:size (t/app s))]
      (is (= [m m] [x y]) "inset by the margin")
      (is (= (- sw m m) w) "the window's width")
      (is (< (+ y h) (- sh m)) "above the status bar"))
    (is (= {:settings/editor-family "Georgia"
            :settings/editor-size   "20"
            :settings/ui-family     "Menlo"
            :settings/ui-size       "13"
            :settings/theme         "default"
            :settings/line-height   "1.3"}
           (settings-values s)))
    (is (= :arrow (app/pointer (t/app s))))
    (is (not (app/caret-visible? (t/app s) (:now @s))) "the caret is hidden")
    (testing "the text takes no input while it is open"
      (t/type! s "i:x")
      (t/press! s sdl/K-RIGHT)
      (t/click! s 30.0 30.0)
      (is (= :normal (:mode (t/app s))))
      (is (= {:text "" :caret 0} (t/doc s)))
      (is (some? (settings-box s))))
    (t/press! s sdl/K-ESCAPE)
    (is (nil? (settings-box s)) "escape closes it")
    (t/type! s "i")
    (is (= :insert (:mode (t/app s))) "and the keys reach the editor again")))

(defn- field-point
  "The middle of the settings window's field `id`, in render pixels."
  [s id]
  (let [{[x y w h] :rect} (some #(when (= id (get-in % [:node :id])) %) (:float-places (t/app s)))]
    [(double (+ x (quot w 2))) (double (+ y (quot h 2)))]))

(defn- click-field! [s id] (apply t/click! s (field-point s id)))

(defn- shown
  "What the settings window's field `id` shows: what was typed into it, or
  else its setting."
  [s id]
  (get-in (t/app s) [:ui-values id] (get (settings-values s) id)))

(deftest editing-font-sizes
  (with-session [s :mode :normal]
    (t/command! s "settings")
    (click-field! s :settings/theme)
    (is (nil? (:focus (t/app s))) "a read-only field takes no focus")
    (click-field! s :settings/editor-size)
    (is (= :settings/editor-size (:focus (t/app s))))
    (is (app/caret-visible? (t/app s) (:now @s)) "the caret is in the field")
    (testing "typing changes the setting, applies it and saves it"
      (t/press! s sdl/K-BACKSPACE)
      (t/press! s sdl/K-BACKSPACE)
      (is (= 20 (get-in (t/app s) [:settings :editor-font :size]))
          "an empty field is no size")
      (is (nil? (:saved-settings @s)))
      (t/type! s "2x4")
      (is (= "24" (get-in (t/app s) [:ui-values :settings/editor-size])) "only digits")
      (is (= 24 (get-in (t/app s) [:settings :editor-font :size])))
      (is (= 24 (get-in @s [:saved-settings :editor-font :size])))
      (is (= 20 (get-in (t/app s) [:fonts :editor-font :size]))
          "the font waits for the setting to be left alone")
      (is (= 150 (app/ms-until-wake (t/app s) (:now @s))) "and wakes up for it")
      (t/advance! s 150)
      (t/send! s {:type :tick})
      (is (= {:editor-font {:family "Georgia" :size 24} :ui-font {:family "Menlo" :size 13}}
             (:fonts (t/app s))) "then it is made again"))
    (testing "up and down step, within the sizes allowed"
      (t/press! s sdl/K-UP)
      (is (= 25 (get-in (t/app s) [:settings :editor-font :size])))
      (t/press! s sdl/K-DOWN)
      (t/press! s sdl/K-DOWN)
      (is (= 23 (get-in (t/app s) [:settings :editor-font :size])))
      (t/type! s "0")
      (is (= 23 (get-in (t/app s) [:settings :editor-font :size])) "230 is too big")
      (t/press! s sdl/K-UP)
      (is (= 72 (get-in (t/app s) [:settings :editor-font :size])) "kept to the largest"))
    (testing "tab moves between the editable fields and the dropdowns"
      (t/press! s sdl/K-TAB)
      (is (= :settings/ui-family (:focus (t/app s))))
      (is (not (app/caret-visible? (t/app s) (:now @s))) "a dropdown has no caret")
      (t/press! s sdl/K-TAB)
      (is (= :settings/ui-size (:focus (t/app s))))
      (t/press! s sdl/K-TAB)
      (is (= :settings/editor-family (:focus (t/app s))) "wrapping around")
      (t/press! s sdl/K-TAB sdl/KMOD-SHIFT)
      (is (= :settings/ui-size (:focus (t/app s)))))
    (testing "given up, a field shows its setting, not what was typed"
      (t/press! s sdl/K-BACKSPACE)
      (is (= "1" (shown s :settings/ui-size)))
      (t/press! s sdl/K-RETURN)
      (is (nil? (:focus (t/app s))))
      (is (= "13" (shown s :settings/ui-size))))
    (testing "escape closes the window from a field"
      (click-field! s :settings/ui-size)
      (t/press! s sdl/K-ESCAPE)
      (is (nil? (:window (t/app s))))
      (is (nil? (:focus (t/app s))))
      (t/type! s "i")
      (is (= :insert (:mode (t/app s))) "the keys reach the editor again"))))

(deftest a-setting-that-cannot-be-saved-says-so
  (with-session [s :mode :normal]
    (swap! s assoc :settings-error "Read-only file system")
    (t/command! s "settings")
    (click-field! s :settings/ui-size)
    (t/press! s sdl/K-UP)
    (is (= 14 (get-in (t/app s) [:settings :ui-font :size])) "it still applies")
    (is (= "Can't save settings: Read-only file system" (:message (t/app s))))))

(deftest settings-given-at-the-start
  (with-session [s :mode :normal]
    (let [a (app/settle (app/create {:renderer (:renderer (:canvas @s))
                                     :settings {:editor-font {:family "Menlo" :size 30}
                                                :ui-font {:family "Menlo" :size 13}}}))]
      (try
        (is (= 30 (get-in a [:fonts :editor-font :size])))
        (finally (app/destroy! a))))))

(deftest the-editor-font-applies-after-the-last-change
  (with-session [s :mode :normal]
    (t/command! s "settings")
    (click-field! s :settings/editor-size)
    (t/press! s sdl/K-UP)
    (t/advance! s 100)
    (t/press! s sdl/K-UP)
    (t/advance! s 100)
    (t/send! s {:type :tick})
    (is (= 20 (get-in (t/app s) [:fonts :editor-font :size])) "each change starts the wait again")
    (t/advance! s 50)
    (t/send! s {:type :tick})
    (is (= 22 (get-in (t/app s) [:fonts :editor-font :size])))))

(deftest the-ui-font-leaves-the-text-laid-out
  (with-session [s :mode :normal]
    (t/command! s "settings")
    (let [{:keys [ctx layout]} (t/app s)]
      (click-field! s :settings/ui-size)
      (t/press! s sdl/K-UP)
      (is (= 14 (get-in (t/app s) [:fonts :ui-font :size])) "it applies straight away")
      (is (identical? ctx (:ctx (t/app s))))
      (is (identical? layout (:layout (t/app s)))))))
;; ---------------------------------------------------------------- dropdowns

(defn- node-of
  "The settings window's box `id`, as placed: {:node :rect :content}."
  [s id]
  (some #(when (= id (get-in % [:node :id])) %) (:float-places (t/app s))))

(defn- list-of [s] (:list (t/app s)))
(defn- list-place [s] (dropdown/place (t/app s)))

(defn- shown-rows
  "The options [i0, i1) wholly in view in the open list."
  [s]
  (let [{:keys [row-h rows]} (list-place s)
        i0 (long (Math/ceil (/ (:scroll (list-of s)) row-h)))]
    [i0 (+ i0 rows (if (zero? (mod (long (:scroll (list-of s))) row-h)) 0 -1))]))

(defn- row-point
  "The middle of the open list's option `i`, as it is scrolled, in render
  pixels."
  [s i]
  (let [{[ix iy iw] :inner :keys [row-h]} (list-place s)]
    [(double (+ ix (quot iw 2)))
     (double (+ iy (* i row-h) (- (:scroll (list-of s))) (quot row-h 2)))]))

(defn- glide-on!
  "Let `ms` pass, in steps as the host's ticks come."
  [s ms]
  (dotimes [_ (quot ms 8)]
    (t/advance! s 8)
    (t/send! s {:type :tick})))

(defn- family [s setting] (get-in (t/app s) [:settings setting :family]))

(deftest the-font-families-are-dropdowns
  (with-session [s :mode :normal]
    (t/command! s "settings")
    (let [{:keys [node]} (node-of s :settings/editor-family)]
      (is (= :dropdown (:kind node)))
      (is (= t/font-families (:options node)) "the fonts installed, as the host finds them")
      (is (:fonts? node) "each shown in its own font"))
    (is (= "Menlo" (:value (:node (node-of s :settings/ui-family)))))
    (testing "a click opens the list, on the font the dropdown holds"
      (click-field! s :settings/editor-family)
      (is (= :settings/editor-family (:focus (t/app s))))
      (is (= 6 (:active (list-of s))) "Georgia")
      (let [{[_ y] :rect :keys [rows max-scroll]} (list-place s)
            {[_ cy _ ch] :rect} (node-of s :settings/editor-family)
            [i0 i1] (shown-rows s)]
        (is (> y (+ cy ch)) "below the dropdown")
        (is (<= rows 8) "no more than 8 options at once")
        (is (pos? max-scroll) "scrolling through the rest")
        (is (< i0 6 (dec i1)) "Georgia shows, in the middle")
        (is (= (:scroll (list-of s)) (:target (list-of s))) "straight away")))
    (is (= :arrow (app/pointer (t/app s))))
    (is (= -1 (app/ms-until-wake (t/app s) (:now @s))) "nothing to wake for")
    (testing "keys move it"
      (t/press! s sdl/K-DOWN)
      (is (= 7 (:active (list-of s))))
      (t/press! s sdl/K-HOME)
      (is (= 0 (:active (list-of s))))
      (t/press! s sdl/K-UP)
      (is (= 0 (:active (list-of s))) "kept to the first")
      (t/press! s sdl/K-END)
      (is (= 14 (:active (list-of s))))
      (t/press! s sdl/K-DOWN)
      (is (= 14 (:active (list-of s))) "and the last")
      (glide-on! s 1000)
      (is (== (:max-scroll (list-place s)) (:scroll (list-of s))) "scrolling to show it")
      (t/press! s sdl/K-PAGEUP)
      (is (= (- 14 (dec (:rows (list-place s)))) (:active (list-of s))) "a list's worth less one")
      (t/press! s sdl/K-PAGEDOWN)
      (is (= 14 (:active (list-of s)))))
    (testing "escape closes the list without choosing, and then the window"
      (t/press! s sdl/K-ESCAPE)
      (is (nil? (list-of s)))
      (is (= :settings (:window (t/app s))))
      (is (= :settings/editor-family (:focus (t/app s))) "the dropdown keeps the focus")
      (is (= "Georgia" (family s :editor-font)))
      (t/press! s sdl/K-ESCAPE)
      (is (nil? (:window (t/app s)))))))

(deftest choosing-a-font-family
  (with-session [s :mode :normal]
    (t/command! s "settings")
    (click-field! s :settings/editor-family)
    (testing "return chooses the option the list is on"
      (t/press! s sdl/K-DOWN)
      (t/press! s sdl/K-RETURN)
      (is (nil? (list-of s)) "closing it")
      (is (= :settings/editor-family (:focus (t/app s))))
      (is (= "Gill Sans" (family s :editor-font)))
      (is (= "Gill Sans" (get-in @s [:saved-settings :editor-font :family])) "saving it")
      (is (= "Gill Sans" (:value (:node (node-of s :settings/editor-family)))))
      (is (= "Georgia" (get-in (t/app s) [:fonts :editor-font :family])) "the font waits")
      (t/advance! s 150)
      (t/send! s {:type :tick})
      (is (= "Gill Sans" (get-in (t/app s) [:fonts :editor-font :family])) "then applies"))
    (testing "with the focus, return, up and down open it again"
      (doseq [k [sdl/K-RETURN sdl/K-UP sdl/K-DOWN]]
        (t/press! s k)
        (is (= 7 (:active (list-of s))))
        (t/press! s sdl/K-ESCAPE)))
    (testing "space chooses"
      (t/type! s " ")
      (is (= 7 (:active (list-of s))) "opening it first")
      (t/press! s sdl/K-UP)
      (t/type! s " ")
      (is (nil? (list-of s)))
      (is (= "Georgia" (family s :editor-font))))
    (testing "a click on an option chooses it"
      (click-field! s :settings/ui-family)
      (is (= :settings/ui-family (:focus (t/app s))))
      (is (= 9 (:active (list-of s))) "Menlo")
      (let [[i0 i1] (shown-rows s)
            i (dec i1)
            option (nth t/font-families i)]
        (apply t/send! s [{:type :move :x (first (row-point s i)) :y (second (row-point s i))}])
        (is (= i (:active (list-of s))) "the pointer moves it")
        (is (< i0 i) "an option further down")
        (apply t/click! s (row-point s i))
        (is (nil? (list-of s)))
        (is (= option (family s :ui-font)))
        (is (= option (get-in (t/app s) [:fonts :ui-font :family])) "the UI font applies at once")))))

(deftest typing-into-a-font-family-list
  (with-session [s :mode :normal]
    (t/command! s "settings")
    (click-field! s :settings/editor-family)
    (t/press! s sdl/K-ESCAPE)
    (testing "typing opens the list, on the first option it begins"
      (t/type! s "t")
      (is (= 12 (:active (list-of s))) "Times New Roman")
      (t/advance! s 500)
      (t/type! s "R")
      (is (= 13 (:active (list-of s))) "Trebuchet MS, ignoring case")
      (t/type! s "x")
      (is (= 13 (:active (list-of s))) "no option begins trx"))
    (testing "letters typed apart start a new name"
      (t/advance! s 1000)
      (t/type! s "c")
      (is (= 4 (:active (list-of s))) "Courier New"))
    (testing "a space in a name is part of it, rather than choosing"
      (t/advance! s 1000)
      (t/type! s "gill ")
      (is (some? (list-of s)))
      (is (= 7 (:active (list-of s))) "Gill Sans")
      (t/advance! s 1000)
      (t/type! s " ")
      (is (nil? (list-of s)))
      (is (= "Gill Sans" (family s :editor-font))))))

(deftest a-font-family-list-scrolls-smoothly
  (with-session [s :mode :normal]
    (t/command! s "settings")
    (click-field! s :settings/editor-family)
    (let [{:keys [row-h max-scroll]} (list-place s)
          s0 (:scroll (list-of s))
          target (min max-scroll (+ s0 (* 3 row-h)))]
      (t/send! s {:type :wheel :dy -1})
      (is (== target (:target (list-of s))) "the wheel heads it down")
      (is (= s0 (:scroll (list-of s))) "but it doesn't jump")
      (is (= 8 (app/ms-until-wake (t/app s) (:now @s))) "it wakes to glide")
      (glide-on! s 48)
      (let [moved (/ (- (:scroll (list-of s)) s0) (- target s0))]
        (is (< 0.5 moved 0.8) "about two thirds of the way in :list-glide-ms"))
      (glide-on! s 400)
      (is (== target (:scroll (list-of s))) "then it arrives")
      (is (= -1 (app/ms-until-wake (t/app s) (:now @s))))
      (t/send! s {:type :wheel :dy -100})
      (is (== max-scroll (:target (list-of s))) "kept within the list")
      (t/send! s {:type :wheel :dy 100})
      (is (zero? (:target (list-of s)))))
    (testing "an option glides under a still pointer"
      (glide-on! s 1000)
      (let [[x y] (row-point s 1)]
        (t/send! s {:type :move :x x :y y})
        (is (= 1 (:active (list-of s))))
        (t/send! s {:type :wheel :dy -1})
        (glide-on! s 1000)
        (is (= 4 (:active (list-of s))) "three rows on")))))

(deftest a-font-family-list-closes
  (with-session [s :mode :normal]
    (t/command! s "settings")
    (testing "on a click elsewhere, which goes no further"
      (click-field! s :settings/editor-family)
      (t/click! s 4.0 4.0)
      (is (nil? (list-of s)))
      (is (= :settings (:window (t/app s))))
      (click-field! s :settings/editor-family)
      (click-field! s :settings/editor-size)
      (is (nil? (list-of s)))
      (is (= :settings/editor-family (:focus (t/app s)))))
    (testing "on a click on its dropdown"
      (click-field! s :settings/editor-family)
      (is (some? (list-of s)))
      (click-field! s :settings/editor-family)
      (is (nil? (list-of s))))
    (testing "as tab moves the focus on"
      (click-field! s :settings/editor-family)
      (t/press! s sdl/K-TAB)
      (is (nil? (list-of s)))
      (is (= :settings/editor-size (:focus (t/app s)))))
    (testing "when the window loses the focus"
      (click-field! s :settings/ui-family)
      (t/send! s {:type :focus :focused? false})
      (is (nil? (list-of s))))
    (is (= "Georgia" (family s :editor-font)))
    (is (= "Menlo" (family s :ui-font)))
    (is (nil? (:saved-settings @s)) "nothing chosen")))

(deftest a-dropdown-shows-the-pointer-over-it
  (with-session [s :mode :normal]
    (t/command! s "settings")
    (let [[x y] (field-point s :settings/ui-family)]
      (t/send! s {:type :move :x x :y y})
      (is (= :settings/ui-family (:ui-hover-id (t/app s))))
      (is (:dirty? (t/app s)) "which shows")
      (t/send! s {:type :move :x 2.0 :y 2.0})
      (is (nil? (:ui-hover-id (t/app s))))
      (t/send! s {:type :move :x x :y y} {:type :leave})
      (is (nil? (:ui-hover-id (t/app s)))))))

(deftest a-font-family-list-fits-the-window
  (doseq [h [140 180 300]
          id [:settings/editor-family :settings/ui-family]]
    (with-session [s :mode :normal :height h]
      (t/command! s "settings")
      (click-field! s id)
      (let [{[_ y _ lh] :rect :keys [rows]} (list-place s)
            {[_ cy _ ch] :rect} (node-of s id)]
        (is (pos? rows))
        (is (<= 0 y) (str h " " id))
        (is (<= (+ y lh) (geo/text-height (t/app s))) "above the status bar")
        (is (or (>= y (+ cy ch)) (<= (+ y lh) cy)) "not over its dropdown")))))

(deftest normal-mode-insert-commands
  (with-session [s :mode :normal]
    (press-i! s)
    (t/type! s "ab\ncd")
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-UP cmd)
    (t/type! s "a")
    (is (= :insert (:mode (t/app s))))
    (is (= 1 (t/caret s)) "a: after the character")
    (t/press! s sdl/K-ESCAPE)
    (t/type! s "A")
    (is (= 2 (t/caret s)) "A: end of line")
    (t/press! s sdl/K-ESCAPE)
    (t/type! s "o")
    (t/type! s "x")
    (is (= "ab\nx\ncd" (t/text s)) "o: a line below")
    (t/press! s sdl/K-ESCAPE)
    (t/type! s "O")
    (t/type! s "y")
    (is (= "ab\ny\nx\ncd" (t/text s)) "O: a line above")))

(deftest normal-mode-selects-and-moves-by-word
  (with-session [s :mode :normal]
    (press-i! s)
    (t/type! s "One two. Three four.")
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-UP cmd)
    (t/press! s sdl/K-RIGHT cmd)
    (is (= 4 (t/caret s)) "cmd+right: next word")
    (t/press! s sdl/K-LEFT cmd)
    (is (= 0 (t/caret s)) "cmd+left: previous word")
    (t/press! s sdl/K-RIGHT cmd)
    (t/type! s "w")
    (is (= "two." (t/selected s)))
    (t/type! s "w")
    (is (= "two. Three" (t/selected s)) "w again extends to the next word")
    (t/type! s "s")
    (is (= "two. Three four." (t/selected s)) "s with a selection extends to the next sentence")))

(deftest s-selects-a-sentence-and-extends
  (with-session [s :mode :normal]
    (press-i! s)
    (t/type! s "One two. Three four. Five.")
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-UP cmd)
    (t/type! s "s")
    (is (= "One two." (t/selected s)))
    (t/type! s "s")
    (is (= "One two. Three four." (t/selected s)))))

(deftest normal-mode-clipboard-and-line-keys
  (with-session [s :mode :normal]
    (press-i! s)
    (t/type! s "  one two")
    (t/press! s sdl/K-ESCAPE)
    (t/type! s "0")
    (is (= 0 (t/caret s)))
    (t/type! s "^")
    (is (= 2 (t/caret s)))
    (t/type! s "w")
    (t/type! s "c")
    (t/set-clipboard! s "x")
    (t/type! s "c")
    (t/type! s "x")
    (is (= "   two" (t/text s)) "x cut the word")
    (t/type! s "p")
    (is (= "  one two" (t/text s)) "p pastes it back")
    (is (= :normal (:mode (t/app s))))))

(deftest ctrl-a-and-e-in-insert-mode
  (with-session [s]
    (t/type! s "one two")
    (t/press! s sdl/K-A sdl/KMOD-CTRL)
    (is (= 0 (t/caret s)))
    (t/press! s sdl/K-E sdl/KMOD-CTRL)
    (is (= 7 (t/caret s)))))

(deftest g-goes-to-a-line
  (with-session [s :mode :normal]
    (press-i! s)
    (t/type! s "one\ntwo\nthree")
    (t/press! s sdl/K-ESCAPE)
    (t/type! s "g")
    (is (= :command (:mode (t/app s))))
    (t/type! s "2x")
    (t/press! s sdl/K-RETURN)
    (is (= :normal (:mode (t/app s))))
    (is (= 4 (t/caret s)))
    (t/type! s "g")
    (t/type! s "99")
    (t/press! s sdl/K-RETURN)
    (is (= 8 (t/caret s)) "clamped to the last line")))

(deftest question-mark-opens-the-help-window
  (with-session [s :mode :normal]
    (t/type! s "?")
    (is (= :help (:window (t/app s))))
    (t/type! s "i")
    (is (= :normal (:mode (t/app s))) "the window takes the input")
    (is (= "" (t/text s)))
    (t/press! s sdl/K-ESCAPE)
    (is (nil? (:window (t/app s))))))

(deftest the-help-window-scrolls-and-fits
  (with-session [s :mode :normal]
    (t/resize! s 400 200)
    (t/type! s "?")
    (let [rows #(count (filter (comp :text :node) (:float-places (t/app s))))]
      (t/press! s sdl/K-DOWN)
      (is (= 1 (:help-scroll (t/app s))))
      (t/press! s sdl/K-END)
      (is (pos? (:help-scroll (t/app s))))
      (t/press! s sdl/K-HOME)
      (is (= 0 (:help-scroll (t/app s))))
      (is (pos? (rows)))
      (let [[_ h] (:size (t/app s))]
        (is (every? (fn [{[_ y _ rh] :rect}] (<= (+ y rh) h)) (:float-places (t/app s)))
            "nothing is placed below the window")))))

(deftest escape-clears-the-selection
  (with-session [s :mode :normal]
    (press-i! s)
    (t/type! s "one two")
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-A cmd)
    (is (some? (t/selected s)))
    (t/press! s sdl/K-ESCAPE)
    (is (nil? (t/selected s)))
    (is (= :normal (:mode (t/app s))))))

(deftest copy-cut-and-paste-say-so
  (with-session [s]
    (t/type! s "one two")
    (t/press! s sdl/K-A cmd)
    (t/press! s sdl/K-C cmd)
    (is (= "Copied 7 characters" (:message (t/app s))))
    (t/press! s sdl/K-X cmd)
    (is (= "Cut 7 characters" (:message (t/app s))))
    (is (= "" (t/text s)))
    (t/press! s sdl/K-V cmd)
    (is (= "Pasted 7 characters" (:message (t/app s))))
    (is (= "one two" (t/text s)))
    (t/press! s sdl/K-ESCAPE)
    (t/type! s "w")
    (t/type! s "x")
    (is (= "Cut 3 characters" (:message (t/app s))))
    (t/type! s "p")
    (is (= "Pasted 3 characters" (:message (t/app s))))))

(deftest undo-and-redo
  (with-session [s :mode :normal]
    (press-i! s)
    (t/type! s "one")
    (t/press! s sdl/K-ESCAPE)
    (press-i! s)
    (t/type! s " two")
    (t/press! s sdl/K-ESCAPE)
    (t/type! s "u")
    (is (= "one" (t/text s)) "typing in a mode undoes as one")
    (t/type! s "u")
    (is (= "" (t/text s)))
    (t/type! s "u")
    (is (= "No further undo information" (:message (t/app s))))
    (t/type! s "r")
    (is (= "one" (t/text s)))
    (t/type! s "r")
    (is (= "one two" (t/text s)))
    (t/type! s "r")
    (is (= "Nothing to redo" (:message (t/app s))))
    (t/type! s "u")
    (is (= "one" (t/text s)) "undo after redo goes back")))

(deftest undo-chain-breaks-on-other-edits
  (with-session [s :mode :normal]
    (press-i! s)
    (t/type! s "a")
    (t/press! s sdl/K-ESCAPE)
    (press-i! s)
    (t/type! s "b")
    (t/press! s sdl/K-ESCAPE)
    (t/type! s "u")
    (t/type! s "u")
    (is (= "" (t/text s)))
    (t/type! s "i")
    (t/type! s "c")
    (t/press! s sdl/K-ESCAPE)
    (t/type! s "r")
    (is (= "Nothing to redo" (:message (t/app s))) "an edit forgets the redos")
    (t/type! s "u")
    (is (= "" (t/text s)))
    (t/type! s "u")
    (is (= "a" (t/text s)) "a fresh undo undoes the undos")))

(deftest undo-restores-cuts-and-pastes
  (with-session [s :mode :normal]
    (press-i! s)
    (t/type! s "one two")
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-A cmd)
    (t/type! s "x")
    (is (= "" (t/text s)))
    (t/type! s "p")
    (t/type! s "u")
    (is (= "" (t/text s)))
    (t/type! s "u")
    (is (= "one two" (t/text s)))))
