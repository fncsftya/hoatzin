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
            [hoatzin.app.help-window :as help-window]
            [hoatzin.app.hints :as hints]
            [hoatzin.app.insets :as insets]
            [hoatzin.app.theme :as theme]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.layout :as layout]
            [hoatzin.lib.text :as text]
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

(deftest a-long-text-rewraps-once-the-width-holds
  (with-session [s :width 400 :height 200]
    (t/type! s (str/join " " (repeat 40 "hoatzin")))
    (swap! s update :app assoc :live-wrap-chars 100)
    (let [lines (layout/line-count (:layout (t/app s)))
          width (get-in (t/app s) [:ctx :width])]
      (t/resize! s 300 200)
      (is (= lines (layout/line-count (:layout (t/app s)))) "not as the window is dragged")
      (is (= width (get-in (t/app s) [:ctx :width])))
      (is (= 120 (app/ms-until-wake (t/app s) (:now @s))) "but waking for it")
      (t/advance! s 100)
      (t/resize! s 200 200)
      (is (= lines (layout/line-count (:layout (t/app s)))) "each new width waits again")
      (t/advance! s 119)
      (t/send! s {:type :tick})
      (is (= lines (layout/line-count (:layout (t/app s)))))
      (t/advance! s 1)
      (t/send! s {:type :tick})
      (is (> (layout/line-count (:layout (t/app s))) lines) "once it has held, it wraps to it")
      (is (nil? (:wrap-at (t/app s))))
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

(deftest the-buffers-window-closes-buffers
  (with-session [s :mode :normal]
    (open! s "/birds/a.txt" "alpha")
    (open! s "/birds/b.txt" "beta")
    (t/command! s "buffers")
    (t/type! s "k")
    (is (= ["scratch" "a.txt"] (buffer-names s)) "k closes the buffer it is on, saved, straight away")
    (is (= :buffers (:window (t/app s))) "the window staying open")
    (is (= 1 (:buffers-active (t/app s))) "on the one in its place")
    (is (= "alpha" (t/text s)) "the buffer before it shown in its place")
    (t/press! s sdl/K-ESCAPE)
    (t/type! s "A")
    (t/type! s "!")
    (t/press! s sdl/K-ESCAPE)
    (t/command! s "buffers")
    (t/type! s "k")
    (is (= "Close \"a.txt\", with unsaved changes? (y/n)" (:prompt (:confirm (t/app s))))
        "with unsaved changes, once asked")
    (t/type! s "n")
    (is (= ["scratch" "a.txt"] (buffer-names s)))
    (t/type! s "k")
    (t/type! s "y")
    (is (= ["scratch"] (buffer-names s)))
    (t/type! s "k")
    (is (= ["scratch"] (buffer-names s)) "the scratch buffer, with no file, without asking")
    (is (= 3 (:buffer-id (t/app s))) "a new one in its place"))
  (testing "a buffer with no file, once asked"
    (with-session [s :mode :normal]
      (t/command! s "new")
      (t/command! s "buffers")
      (t/type! s "k")
      (is (= "Close \"untitled\", which has no file? (y/n)" (:prompt (:confirm (t/app s)))))
      (t/type! s "y")
      (is (= ["scratch"] (buffer-names s)))
      (testing "and the one it is on need not be the current one"
        (t/press! s sdl/K-ESCAPE)
        (t/command! s "new")
        (open! s "/birds/a.txt" "alpha")
        (t/command! s "buffers")
        (t/press! s sdl/K-UP)
        (t/type! s "k")
        (t/type! s "y")
        (is (= ["scratch" "a.txt"] (buffer-names s)))
        (is (= "alpha" (t/text s)) "the current one stays shown")))))

(defn- row-texts
  "The texts of the labels in the buffers window's row for buffer `i`."
  [s i]
  (let [row (some #(when (= i (get-in % [:node :buffer-row])) (:node %)) (:float-places (t/app s)))]
    (mapv :text (:children row))))

(deftest the-buffers-window-shows-unsaved-buffers
  (with-session [s :mode :normal :dir "/home"]
    (open! s "/birds/a.txt" "alpha")
    (t/command! s "buffers")
    (is (= ["" "scratch" "*unsaved*"] (row-texts s 0)) "not the working directory")
    (is (= ["•" "a.txt" "/birds/a.txt"] (row-texts s 1)))))

(deftest the-buffers-window-previews
  (with-session [s :mode :normal]
    (open! s "/birds/a.txt" "alpha\nbeta")
    (t/command! s "buffers")
    (t/press! s sdl/K-UP)
    (t/type! s "p")
    (is (= 0 (:buffers-preview (t/app s))))
    (let [texts (set (keep #(get-in % [:node :text]) (:float-places (t/app s))))]
      (is (contains? texts "Preview: scratch"))
      (is (contains? texts "(empty)")))
    (t/press! s sdl/K-DOWN)
    (t/type! s "k")
    (is (= 0 (:buffers-active (t/app s))) "the keys wait")
    (is (= ["scratch" "a.txt"] (buffer-names s)))
    (t/press! s sdl/K-ESCAPE)
    (is (nil? (:buffers-preview (t/app s))) "escape ends it")
    (is (= :buffers (:window (t/app s))) "and leaves the window open")
    (t/press! s sdl/K-DOWN)
    (t/type! s "p")
    (let [texts (set (keep #(get-in % [:node :text]) (:float-places (t/app s))))]
      (is (contains? texts "alpha"))
      (is (contains? texts "beta")))))

(deftest the-buffers-window-tags
  (with-session [s :mode :normal]
    (open! s "/birds/a.txt" "alpha")
    (t/command! s "buffers")
    (t/type! s "t")
    (t/type! s "todo")
    (is (= ["•" "a.txt" "/birds/a.txt" "todo|"] (row-texts s 1)) "typed in the row")
    (t/press! s sdl/K-BACKSPACE)
    (t/press! s sdl/K-UP)
    (is (= 1 (:buffers-active (t/app s))) "the window stays on it")
    (t/press! s sdl/K-RETURN)
    (is (= ["•" "a.txt" "/birds/a.txt" "tod"] (row-texts s 1)))
    (is (= :buffers (:window (t/app s))) "return keeps it")
    (is (= "tod" (:tag (t/app s))))
    (let [[r g b] (:tag-color (t/app s))]
      (is (every? #(< 150 % 256) [r g b]) "in a pastel colour"))
    (t/type! s "t")
    (t/type! s "x")
    (t/press! s sdl/K-ESCAPE)
    (is (= "tod" (:tag (t/app s))) "escape gives it up")
    (is (= :buffers (:window (t/app s))))
    (t/type! s "t")
    (t/type! s "done")
    (t/press! s sdl/K-RETURN)
    (is (= "done" (:tag (t/app s))) "t again overwrites it")
    (t/type! s "t")
    (t/type! s "a very long tag indeed")
    (t/press! s sdl/K-RETURN)
    (is (= "a very long tag" (:tag (t/app s))) "a short one")
    (t/press! s sdl/K-UP)
    (t/type! s "tbird")
    (t/press! s sdl/K-RETURN)
    (is (= "bird" (:tag (first (buffers/listing (t/app s))))) "a buffer not shown is tagged too")
    (t/press! s sdl/K-DOWN)
    (t/type! s "t")
    (t/press! s sdl/K-RETURN)
    (is (nil? (:tag (t/app s))) "with nothing typed, the tag goes")))

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

;; ---------------------------------------------------------------- major modes

(deftest an-auk-file-is-read-in-auk-mode
  (with-session [s :mode :normal]
    (open! s "/notes/birds.auk" "{:content [\"hoatzin\" \"\" \"kakapo \\\"owl\\\" parrot\"]}")
    (is (= "hoatzin\n\nkakapo \"owl\" parrot" (t/text s)) "each string of :content a line")
    (is (= "auk" (:major-mode (t/app s))))
    (is (not (:modified? (t/app s))))
    (is (= "\"birds.auk\" 3 lines" (:message (t/app s))))
    (open! s "/notes/birds.txt" "plain")
    (is (nil? (:major-mode (t/app s))) "a file of no mode's extension is in none")
    (open! s "/notes/birds.auk" "")
    (is (= "auk" (:major-mode (t/app s))) "each buffer keeps its mode")))

(deftest an-auk-file-is-written-as-edn
  (with-session [s :mode :normal]
    (open! s "/notes/birds.auk" "{:content [\"hoatzin\"]}")
    (t/type! s "A")
    (t/press! s sdl/K-RETURN)
    (t/type! s "a \"quoted\" kea")
    (t/press! s sdl/K-ESCAPE)
    (t/command! s "w")
    (is (= "{:content\n [\"hoatzin\"\n  \"a \\\"quoted\\\" kea\"]}\n" (get-in @s [:files "/notes/birds.auk"])))
    (is (not (:modified? (t/app s))))
    (testing "and reads back as it was"
      (open! s "/notes/copy.auk" (get-in @s [:files "/notes/birds.auk"]))
      (is (= "hoatzin\na \"quoted\" kea" (t/text s))))))

(deftest an-auk-file-that-is-not-one-is-refused
  (with-session [s :mode :normal]
    (doseq [[contents why] [["{:content" "EOF while reading"]
                            ["[\"a\"]" "not an auk file: expected a map"]
                            ["{:content [:heading]}" ":content must be a vector of strings"]
                            ["{:content [] :title \"x\"}" "unsupported keys :title"]]]
      (open! s "/notes/bad.auk" contents)
      (is (str/starts-with? (:message (t/app s)) (str "Can't open bad.auk: " why)) contents)
      (is (= ["scratch"] (buffer-names s)) "and opens no buffer"))))

(deftest reverting-an-auk-file-reads-it-in-its-mode
  (with-session [s :mode :normal]
    (swap! s assoc-in [:files "/notes/a.auk"] "{:content [\"one\" \"two\"]}")
    (open! s "/notes/a.auk" "{:content [\"zero\"]}")
    (t/command! s "revert")
    (is (= "one\ntwo" (t/text s)))))

(deftest the-mode-command-chooses-the-buffers-mode
  (with-session [s :mode :normal]
    (t/command! s "mode")
    (is (= "Mode text; modes: text, auk" (:message (t/app s))))
    (t/command! s "mode auk")
    (is (= "auk" (:major-mode (t/app s))))
    (is (= "Auk mode" (:message (t/app s))))
    (t/command! s "mo heron")
    (is (= "No such mode: heron" (:message (t/app s))))
    (is (= "auk" (:major-mode (t/app s))) "and stays in the one it was in")
    (t/command! s "mode text")
    (is (nil? (:major-mode (t/app s))))
    (testing "tab completes the mode's name"
      (t/type! s ":mode a")
      (t/press! s sdl/K-TAB)
      (is (= "mode auk" (:command (t/app s))))
      (t/press! s sdl/K-RETURN)
      (is (= "auk" (:major-mode (t/app s))))
      (t/type! s ":mo t")
      (t/press! s sdl/K-TAB)
      (is (= "mo text" (:command (t/app s))) "after a command's prefix too")
      (t/press! s sdl/K-ESCAPE)
      (t/type! s ":mode x")
      (t/press! s sdl/K-TAB)
      (is (= "mode x" (:command (t/app s))) "a name no mode's begins stays as it is")
      (t/press! s sdl/K-ESCAPE)
      (t/type! s ":minor v")
      (t/press! s sdl/K-TAB)
      (is (= "minor variants" (:command (t/app s))) "and the minor modes' for :minor")
      (t/press! s sdl/K-ESCAPE)
      (t/type! s ":open fo")
      (t/press! s sdl/K-TAB)
      (is (= "open fo" (:command (t/app s))) "a command taking anything leaves it alone")
      (t/press! s sdl/K-ESCAPE))))

(deftest a-buffer-saved-as-an-auk-file-is-written-in-auk-mode
  (testing "taking the mode from where it is saved"
    (with-session [s]
      (t/type! s "one")
      (t/press! s sdl/K-ESCAPE)
      (t/command! s "write")
      (t/send! s {:type :save-chosen :path "/notes/new.auk"})
      (is (= "{:content\n [\"one\"]}\n" (get-in @s [:files "/notes/new.auk"])))
      (is (= "auk" (:major-mode (t/app s))))))
  (testing "or keeping the one it is in"
    (with-session [s]
      (t/type! s "one")
      (t/press! s sdl/K-ESCAPE)
      (t/command! s "mode auk")
      (t/command! s "write")
      (t/send! s {:type :save-chosen :path "/notes/new.txt"})
      (is (= "{:content\n [\"one\"]}\n" (get-in @s [:files "/notes/new.txt"]))))))

(def ^:private heron-mode
  "A mode of the user's: `x` says hello rather than cutting, and :shout
  shouts its argument."
  ["heron.clj"
   "(ns heron (:require [clojure.string :as str] [hoatzin.mode :as mode]))
    {:name \"heron\"
     :extensions [\"heron\"]
     :normal {\"x\" (fn [app _] (mode/message app (str \"hello, \" (mode/text app))))
              \"!\" (fn [app _] (throw (ex-info \"broken\" {})))}
     :commands {\"shout\" (fn [app _ force? arg]
                            (mode/message app (str (str/upper-case arg) (when force? \"!\"))))
                \"open\" (fn [app _ _ _] (mode/message app \"no opening herons\"))}}"])

(deftest a-mode-binds-keys-and-commands
  (with-session [s :mode :normal :mode-sources [heron-mode]]
    (open! s "/birds/grey.heron" "grey")
    (is (= "heron" (:major-mode (t/app s))))
    (t/type! s "w")
    (t/type! s "x")
    (is (= "hello, grey" (:message (t/app s))) "over the editor's own")
    (is (= "grey" (t/text s)) "which it replaces")
    (t/command! s "shout quietly please")
    (is (= "QUIETLY PLEASE" (:message (t/app s))))
    (t/command! s "shout! hi")
    (is (= "HI!" (:message (t/app s))))
    (t/command! s "open")
    (is (= "no opening herons" (:message (t/app s))))
    (is (zero? (:dialogs @s)))
    (t/type! s "!")
    (is (= "broken (heron mode)" (:message (t/app s))) "a mode's mistake says so")
    (t/command! s "mode text")
    (t/type! s "x")
    (is (= "" (t/text s)) "out of the mode, the editor's own keys")
    (t/command! s "shout")
    (is (= "Not an editor command: shout" (:message (t/app s))))))

(deftest a-mode-that-cannot-be-loaded-says-why
  (with-session [s :mode-sources [["broken.clj" "(+ 1"] ["nameless.clj" "{}"]]]
    (is (= (str "Can't load mode broken.clj: EOF while reading, expected ) to match ( at [1,1]; "
                "Can't load mode nameless.clj: nameless.clj: not a mode, a map with a :name")
           (:message (t/app s))))
    (is (= ["auk" "search" "variants"] (sort (keys (:modes (t/app s))))) "the others load")))

;; ---------------------------------------------------------------- auk sections

(defn- tree
  "Insets as `insets/snapshot` gives them, as [after text] or [after text
  [inner ...]]."
  [specs]
  (mapv (fn [{:keys [after text insets]}] (cond-> [after text] (seq insets) (conj (tree insets)))) specs))

(defn- sections [s] (tree (insets/snapshot (t/app s))))

(defn- in-section
  "The text of the section the caret is in, or nil."
  [s]
  (some-> (insets/innermost (t/app s)) :doc :text str))

(defn- section-caret [s] (:caret (:doc (insets/innermost (t/app s)))))

(defn- section-place [s id] (insets/place-of (t/app s) id))

(defn- auk! [s data] (open! s "/notes/n.auk" (pr-str data)))

(defn- ref [id] {:type :section :ref id})

(deftest auk-reads-and-writes-sections
  (with-session [s :mode :normal]
    (auk! s {:content [(ref 7) "one" (ref 3) "two"]
             :sections [{:id 3 :content ["below one" (ref 5)]}
                        {:id 5 :content ["inner" "of two lines"]}
                        {:id 7 :content ["above"]}]})
    (is (= "one\ntwo" (t/text s)) "the strings are the text")
    (is (= [[-1 "above"] [0 "below one" [[0 "inner\nof two lines"]]]] (sections s))
        "the sections are where :content has them, and theirs where theirs does")
    (is (not (:modified? (t/app s))))
    (t/command! s "w")
    (is (= (str "{:content\n"
                " [{:type :section :ref 1}\n"
                "  \"one\"\n"
                "  {:type :section :ref 2}\n"
                "  \"two\"]\n"
                " :sections\n"
                " [{:id 1 :content [\"above\"]}\n"
                "  {:id 2 :content [\"below one\" {:type :section :ref 3}]}\n"
                "  {:id 3 :content [\"inner\" \"of two lines\"]}]}\n")
           (get-in @s [:files "/notes/n.auk"]))
        "written back numbered as they come")
    (testing "a note of nothing but sections"
      (open! s "/notes/alone.auk" (pr-str {:content [(ref 1)] :sections [{:id 1 :content ["alone"]}]}))
      (t/command! s "w")
      (is (= "{:content\n [{:type :section :ref 1}]\n :sections\n [{:id 1 :content [\"alone\"]}]}\n"
             (get-in @s [:files "/notes/alone.auk"]))))))

(deftest auk-refuses-sections-it-cannot-place
  (with-session [s :mode :normal]
    (doseq [[data why] [[{:content [(ref 1)]} "no section 1"]
                        [{:content [] :sections [{:id 1 :content ["x"]}]} "section 1 is not referred to"]
                        [{:content [(ref 1) (ref 1)] :sections [{:id 1 :content ["x"]}]}
                         "section 1 is referred to twice"]
                        [{:content [(ref 1)] :sections [{:id 1 :content [(ref 1)]}]}
                         "section 1 is referred to twice"]
                        [{:content [(ref 1)] :sections [{:id 1 :content "x"}]}
                         "section 1's :content must be a vector of strings"]
                        [{:content [{:type :section :ref 1 :open? true}] :sections [{:id 1 :content ["x"]}]}
                         ":content must be a vector of strings"]
                        [{:content [{:type :list :content [{:text "a" :checked? true}]}]}
                         ":content must be a vector of strings"]
                        [{:content [{:type :checklist :content [{:text "a" :checked? "yes"}]}]}
                         ":content must be a vector of strings"]
                        [{:content [(ref 1)] :sections [{:id 1 :title 2 :content ["x"]}]}
                         "section 1's :title must be a string"]]]
      (auk! s data)
      (is (str/starts-with? (:message (t/app s)) (str "Can't open n.auk: " why)) (pr-str data)))
    (is (= ["scratch"] (buffer-names s)))))

(deftest cmd-s-adds-a-section-below-the-line
  (with-session [s :mode :normal]
    (auk! s {:content ["one" "two"]})
    (t/press! s sdl/K-S cmd)
    (is (= [[0 ""]] (sections s)) "below the line the caret is in")
    (is (= "one\ntwo" (t/text s)) "with no new line after it: there is a line there already")
    (is (= "" (in-section s)) "and the caret in it")
    (is (= :normal (:mode (t/app s))) "still in normal mode")
    (is (:modified? (t/app s)))
    (t/type! s "i")
    (t/type! s "a note")
    (is (= "a note" (in-section s)) "typing goes into the section")
    (is (= "one\ntwo" (t/text s)) "and leaves the text alone")
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-S cmd)
    (is (= [[0 "a note\n" [[0 ""]]]] (sections s)) "in a section, a new one goes in it")
    (is (= "" (in-section s)) "with the caret in that")
    (t/command! s "w")
    (is (= (str "{:content\n [\"one\"\n  {:type :section :ref 1}\n  \"two\"]\n"
                " :sections\n [{:id 1 :content [\"a note\" {:type :section :ref 2} \"\"]}\n"
                "  {:id 2 :content [\"\"]}]}\n")
           (get-in @s [:files "/notes/n.auk"])))
    (is (not (:modified? (t/app s))))
    (testing "only in auk mode"
      (t/command! s "mode text")
      (t/press! s sdl/K-S cmd)
      (is (= [[0 "a note\n" [[0 ""]]]] (sections s))))))

(deftest a-section-on-the-last-line-makes-a-line-after-it
  (with-session [s :mode :normal]
    (auk! s {:content ["one"]})
    (t/press! s sdl/K-S cmd)
    (is (= "one\n" (t/text s)) "a new line to go on writing in")))

(deftest the-line-after-a-section-is-where-the-next-goes
  (with-session [s :mode :normal]
    (auk! s {:content ["one"]})
    (t/press! s sdl/K-S cmd)
    (t/press! s sdl/K-DOWN)
    (is (nil? (in-section s)) "down from the section")
    (is (= 4 (t/caret s)) "is the new line after it")
    (t/press! s sdl/K-S cmd)
    (is (= [[0 ""] [0 ""]] (sections s)) "on an empty line, a section takes its place")
    (is (= "one\n" (t/text s)) "and adds no line: the empty one is after it")
    (t/press! s sdl/K-DOWN)
    (is (= 4 (t/caret s)))))

(deftest a-section-on-an-empty-first-line-goes-above-it
  (with-session [s :mode :normal]
    (auk! s {:content ["" "two"]})
    (t/press! s sdl/K-S cmd)
    (is (= [[-1 ""]] (sections s)))
    (is (= "\ntwo" (t/text s)))))

(deftest a-section-grows-to-ten-lines-then-scrolls
  (with-session [s :mode :normal]
    (auk! s {:content ["one"]})
    (t/press! s sdl/K-S cmd)
    (t/type! s "i")
    (let [lh (layout/line-height (:layout (t/app s)))
          id (:inset (t/app s))
          body-h #(let [[_ _ _ h] (:text (section-place s id))] h)]
      (is (= lh (body-h)) "one line to begin with")
      (t/type! s "1\n2\n3")
      (is (= (* 3 lh) (body-h)) "a line more for each")
      (t/type! s (apply str (map #(str "\n" %) (range 4 16))))
      (is (= (* insets/max-rows lh) (body-h)) "no more than ten")
      (is (= (* 5 lh) (get-in (t/app s) [:insets id :scroll])) "scrolled to show the caret's line")
      (t/press! s sdl/K-UP cmd)
      (is (zero? (get-in (t/app s) [:insets id :scroll])) "and back up with it"))))

(deftest a-section-in-a-section-makes-room-in-it
  (with-session [s :mode :normal]
    (auk! s {:content ["one" (ref 1)] :sections [{:id 1 :content ["outer" (ref 2)]} {:id 2 :content ["inner"]}]})
    (let [lh (layout/line-height (:layout (t/app s)))
          [_ _ _ outer-h] (:text (section-place s 0))
          inner-h (:height (first (:block-places (get-in (t/app s) [:insets 0]))))]
      (is (= (+ lh inner-h) outer-h) "the outer section is its line and the inner one")
      (t/press! s sdl/K-DOWN)
      (t/press! s sdl/K-DOWN)
      (is (= "inner" (in-section s)))
      (t/type! s "A")
      (t/type! s "\nmore")
      (let [[_ _ _ h] (:text (section-place s 0))]
        (is (= (+ outer-h lh) h) "and grows as the inner one does")))))

(deftest up-and-down-cross-into-and-out-of-sections
  (with-session [s :mode :normal]
    (auk! s {:content ["one" (ref 1) "two"] :sections [{:id 1 :content ["a" "b"]}]})
    (t/press! s sdl/K-DOWN)
    (is (= "a\nb" (in-section s)) "down from the line above goes in")
    (is (zero? (section-caret s)) "at its first line")
    (t/press! s sdl/K-DOWN)
    (is (= 2 (section-caret s)) "down within it")
    (t/press! s sdl/K-DOWN)
    (is (nil? (in-section s)) "down from its last line comes out")
    (is (= 4 (t/caret s)) "on the line below")
    (t/press! s sdl/K-UP)
    (is (= 2 (section-caret s)) "up from the line below goes in at its last line")
    (t/press! s sdl/K-UP)
    (t/press! s sdl/K-UP)
    (is (nil? (in-section s)) "and up from its first comes out")
    (is (= 0 (t/caret s)))
    (testing "the caret stops over a folded section"
      (let [[x y _ h] (:header (section-place s 0))]
        (t/click! s (+ 48.0 x 10) (+ 48.0 y (quot h 2))))
      (is (get-in (t/app s) [:insets 0 :collapsed?]))
      (t/press! s sdl/K-DOWN)
      (is (insets/over (t/app s)) "on the way down")
      (t/press! s sdl/K-DOWN)
      (is (nil? (insets/path (t/app s))))
      (is (= 4 (t/caret s)) "and goes on to the line below")
      (t/press! s sdl/K-UP)
      (is (insets/over (t/app s)) "and on the way up")
      (t/press! s sdl/K-UP)
      (is (= 0 (t/caret s))))))

(defn- stop
  "Where the caret is, as up and down see it: the text it is in, or the
  folded section it is over, and that text's line."
  [s]
  (let [a (t/app s)
        v (insets/innermost-view a)]
    (if (insets/over a)
      [(insets/path a) :header]
      [(insets/path a) (str (text/line (text/of (get-in v [:doc :text]))
                                       (first (text/line-at (text/of (get-in v [:doc :text]))
                                                            (get-in v [:doc :caret])))))])))

(deftest up-and-down-move-a-visual-line-at-a-time
  ;; a list above the first line; a checklist at the end of a list, just
  ;; before another; a section ending in a list in a list; and a folded
  ;; section, with a list above its first line
  (with-session [s :mode :normal :height 2000]
    (auk! s {:content [{:type :list :content [{:text "z"}]}
                       "one"
                       {:type :list :content [{:text "a"} {:type :checklist :content [{:text "x"} {:text "y"}]}]}
                       {:type :checklist :content [{:text "p"} {:text "q"}]}
                       (ref 1) "two" (ref 2) "three"]
             :sections [{:id 1 :content ["s" {:type :list :content [{:text "m"} {:type :list :content [{:text "n"}]}]}]}
                        {:id 2 :content [{:type :list :content [{:text "above"}]} "t"]}]})
    (let [[x y _ h] (:header (section-place s 4))]
      (t/click! s (+ 48.0 x 10) (+ 48.0 y (quot h 2))))
    (t/press! s sdl/K-UP cmd)
    (t/press! s sdl/K-UP)
    (let [top #(insets/caret-top (t/app s))
          walk (fn [key n] (vec (for [_ (range n)] (do (t/press! s key) [(stop s) (top)]))))
          start [(stop s) (top)]
          down (walk sdl/K-DOWN 12)
          up   (walk sdl/K-UP 12)]
      (is (= [[[0] "z"] [nil "one"] [[1] "a"] [[1 0] "x"] [[1 0] "y"] [[2] "p"] [[2] "q"]
              [[3] "s"] [[3 0] "m"] [[3 0 0] "n"] [nil "two"] [[4] :header] [nil "three"]]
             (into [(first start)] (map first down)))
          "every line, and the folded header, in order down the page")
      (is (apply < (second start) (map second down)) "each lower than the last")
      (is (= (reverse (into [start] (butlast down))) up) "and back up the same way")
      (t/press! s sdl/K-UP)
      (is (= [[0] :header] (stop s)) "above the first line, the caret goes before the list it is in")
      (is (insets/before (t/app s)))
      (t/press! s sdl/K-UP)
      (is (= [[0] :header] (stop s)) "and there is nothing above that"))))

(deftest up-goes-before-an-inset-with-nothing-above
  (with-session [s :mode :normal]
    (auk! s {:content [(ref 1) "after"] :sections [{:id 1 :content ["in"]}]})
    (t/press! s sdl/K-UP)
    (is (= [0] (insets/path (t/app s))) "up from the first line goes into the section")
    (is (not (insets/over (t/app s))))
    (t/press! s sdl/K-UP)
    (is (= [0] (insets/path (t/app s))))
    (is (some? (insets/before (t/app s))) "and up again, before it")
    (t/press! s sdl/K-UP)
    (is (some? (insets/before (t/app s))) "where it stays")
    (t/press! s sdl/K-DOWN)
    (is (nil? (insets/before (t/app s))) "down goes into the section again")
    (is (= "in" (in-section s)))
    (t/press! s sdl/K-UP)
    (is (some? (insets/before (t/app s))))
    (testing "cmd+shift+o makes a line above it, to write in"
      (t/press! s sdl/K-O (bit-or cmd sdl/KMOD-SHIFT))
      (is (= :insert (:mode (t/app s))))
      (is (nil? (insets/path (t/app s))))
      (is (= "\nafter" (t/text s)))
      (is (= 0 (t/caret s)))
      (is (= [[0 "in"]] (sections s)) "the section is below the new line")
      (t/type! s "new")
      (is (= "new\nafter" (t/text s))))))

(deftest cmd-shift-o-opens-a-line-above-a-section-or-list
  (with-session [s :mode :normal]
    (auk! s {:content ["one" {:type :list :content [{:text "a"}]} "two"]})
    (t/press! s sdl/K-DOWN)
    (is (= [0] (insets/path (t/app s))))
    (t/press! s sdl/K-O (bit-or cmd sdl/KMOD-SHIFT))
    (is (= "one\n\ntwo" (t/text s)) "a line between the line above the list and the list")
    (is (= [[1 "a"]] (sections s)) "the list below it")
    (is (= 4 (t/caret s)))
    (is (= :insert (:mode (t/app s)))))
  (testing "on a normal line it is shift+o"
    (with-session [s :mode :normal]
      (auk! s {:content ["one" "two"]})
      (t/press! s sdl/K-O (bit-or cmd sdl/KMOD-SHIFT))
      (is (= "\none\ntwo" (t/text s)))
      (is (= :insert (:mode (t/app s)))))))

(defn- selections
  "Each text's selection, the buffer's first, then the insets' in order:
  what each shows."
  [s]
  (letfn [(walk [level]
            (cons (ed/selected-text (:doc level))
                  (mapcat (fn [{:keys [id]}] (walk (get-in level [:insets id])))
                          (insets/ordered level))))]
    (walk (t/app s))))

(deftest selecting-down-through-a-list
  (with-session [s :mode :normal]
    (auk! s {:content ["one" {:type :list :content [{:text "a"} {:text "b"}]} "two"]})
    (t/press! s sdl/K-DOWN sdl/KMOD-SHIFT)
    (is (= ["one" nil] (map #(some-> % str) (selections s)))
        "into the list: all of the line above it")
    (t/press! s sdl/K-DOWN sdl/KMOD-SHIFT)
    (is (= ["one" "a\n"] (selections s)))
    (t/press! s sdl/K-DOWN sdl/KMOD-SHIFT)
    (is (= ["one\n" "a\nb"] (selections s)) "and out of it, the list is in the selection")
    (t/press! s sdl/K-DOWN sdl/KMOD-SHIFT)
    (is (= ["one\ntwo" "a\nb"] (selections s)))
    (t/press! s sdl/K-UP sdl/KMOD-SHIFT)
    (is (= ["one" "a\nb"] (selections s)) "and back, into the list from below")
    (t/press! s sdl/K-ESCAPE)
    (is (every? nil? (selections s)) "escape gives it all up")))

(deftest selecting-through-a-section-has-one-selection
  (with-session [s :mode :normal]
    (auk! s {:content ["one" (ref 1) "two"] :sections [{:id 1 :content ["in" "side"]}]})
    (t/press! s sdl/K-RIGHT sdl/KMOD-SHIFT)
    (is (= ["o" nil] (selections s)))
    (t/press! s sdl/K-END sdl/KMOD-SHIFT)
    (t/press! s sdl/K-RIGHT sdl/KMOD-SHIFT)
    (is (= [[0] "in\nside"] [(insets/path (t/app s)) (in-section s)]) "the caret goes into the section")
    (is (= ["one" nil] (selections s)))
    (t/press! s sdl/K-DOWN sdl/KMOD-SHIFT)
    (t/press! s sdl/K-END sdl/KMOD-SHIFT)
    (is (= ["one" "in\nside"] (selections s)))
    (t/press! s sdl/K-RIGHT sdl/KMOD-SHIFT)
    (is (nil? (insets/path (t/app s))) "right at the end of the section goes on to the text after")
    (is (= ["one\n" "in\nside"] (selections s)))
    (t/press! s sdl/K-LEFT sdl/KMOD-SHIFT)
    (t/press! s sdl/K-LEFT sdl/KMOD-SHIFT)
    (is (= ["one" "in\nsid"] (selections s)) "and back, a character at a time")
    (testing "it is one selection to copy"
      (t/press! s sdl/K-C cmd)
      (is (= "one\nin\nsid" (:clipboard @s))))
    (testing "a movement without shift gives it up everywhere"
      (t/press! s sdl/K-LEFT)
      (is (every? nil? (selections s))))))

(deftest select-all-takes-in-the-insets
  (with-session [s :mode :normal]
    (auk! s {:content ["one" {:type :list :content [{:text "a"}]} (ref 1) "two"]
             :sections [{:id 1 :content ["in" (ref 2)]} {:id 2 :content ["deep"]}]})
    (t/press! s sdl/K-A cmd)
    (is (= ["one\ntwo" "a" "in" "deep"] (selections s)) "every text, whole")
    (t/press! s sdl/K-C cmd)
    (is (= "one\na\nin\ndeep\ntwo" (:clipboard @s)))
    (testing "cmd+shift+a selects what is in the inset the caret is in"
      (t/press! s sdl/K-ESCAPE)
      (t/press! s sdl/K-UP cmd)
      (t/press! s sdl/K-DOWN)
      (t/press! s sdl/K-DOWN)
      (is (= "in" (in-section s)))
      (t/press! s sdl/K-A (bit-or cmd sdl/KMOD-SHIFT))
      (is (= [nil nil "in" "deep"] (selections s)))
      (t/press! s sdl/K-C cmd)
      (is (= "in\ndeep" (:clipboard @s))))
    (testing "and with the caret in no inset, all"
      (t/press! s sdl/K-ESCAPE)
      (dotimes [_ 6] (t/press! s sdl/K-UP))
      (is (nil? (insets/path (t/app s))))
      (t/press! s sdl/K-A (bit-or cmd sdl/KMOD-SHIFT))
      (is (= ["one\ntwo" "a" "in" "deep"] (selections s))))))

(deftest deleting-a-selection-across-texts
  (with-session [s :mode :normal]
    (auk! s {:content ["one" {:type :list :content [{:text "a"}]} (ref 1) "two"]
             :sections [{:id 1 :content ["inner"]}]})
    (press-i! s)
    (t/press! s sdl/K-A cmd)
    (t/press! s sdl/K-BACKSPACE)
    (is (= "" (t/text s)))
    (is (= [] (sections s)) "the insets that were all of it go")
    (is (every? nil? (selections s)))
    (t/type! s "x")
    (is (= "x" (t/text s)) "and typing goes on where it began"))
  (testing "a part of it"
    (with-session [s :mode :normal]
      (auk! s {:content ["one" (ref 1) "two"] :sections [{:id 1 :content ["inner" "text"]}]})
      (press-i! s)
      (t/press! s sdl/K-RIGHT)
      (dotimes [_ 8] (t/press! s sdl/K-RIGHT sdl/KMOD-SHIFT))
      (is (= ["ne" "inner"] (map str (remove nil? (selections s)))) "ne, and the section's first line")
      (t/press! s sdl/K-BACKSPACE)
      (is (= "o\ntwo" (t/text s)))
      (is (= [[0 "\ntext"]] (sections s)) "the section is as it was but for what was selected of it"))))

(deftest a-modifier-pressed-alone-keeps-the-selection
  (with-session [s :mode :normal]
    (auk! s {:content ["one" {:type :list :content [{:text "a"}]} "two"]})
    (t/press! s sdl/K-A (bit-or cmd sdl/KMOD-SHIFT))
    (is (= ["one\ntwo" "a"] (selections s)))
    (doseq [k [0x400000e3 0x400000e1 0x400000e0]]
      (t/press! s k))
    (is (= ["one\ntwo" "a"] (selections s)) "cmd, shift and ctrl, as they come and go")))

(deftest dragging-selects-across-insets
  (with-session [s :mode :normal :height 600]
    (auk! s {:content ["one" {:type :list :content [{:text "a"} {:text "b"}]} (ref 1) "two"]
             :sections [{:id 1 :content ["in" "side"]}]})
    (let [a  (t/app s)
          lh (layout/line-height (:layout a))
          y  (fn [k] (+ 48.0 (geo/line-top a k) (quot lh 2)))
          [lx ly] (let [[bx by bw bh] (:text (insets/place-of a 0))] [(+ 48.0 bx 4) (+ 48.0 by (quot lh 2))])]
      (t/drag! s [49.0 (y 0)] [(+ 48.0 400) (y 1)])
      (is (= ["one\ntwo" "a\nb" "in\nside"] (selections s)) "down to the line after them: all of the insets between")
      (t/drag! s [49.0 (y 0)] [lx ly])
      (is (= ["one" nil nil] (selections s)) "into the list, at its start")
      (t/press! s sdl/K-C cmd)
      (is (= "one\n" (:clipboard @s))))))

(deftest backspace-moves-a-line-up-into-the-inset-above
  (testing "an empty line"
    (with-session [s :mode :normal]
      (auk! s {:content ["one" {:type :list :content [{:text "a"} {:text "b"}]} "" "two"]})
      (press-i! s)
      (t/press! s sdl/K-DOWN) (t/press! s sdl/K-DOWN) (t/press! s sdl/K-DOWN)
      (is (nil? (insets/path (t/app s))))
      (is (= 4 (t/caret s)) "on the empty line")
      (t/press! s sdl/K-BACKSPACE)
      (is (= "one\ntwo" (t/text s)))
      (is (= [[0 "a\nb"]] (sections s)))
      (is (= [[0] 3] [(insets/path (t/app s)) (section-caret s)]) "the caret at the end of the list")))
  (testing "a line with text joins the last line"
    (with-session [s :mode :normal]
      (auk! s {:content ["one" (ref 1) "two"] :sections [{:id 1 :content ["in" (ref 2)]} {:id 2 :content ["deep"]}]})
      (press-i! s)
      (t/press! s sdl/K-DOWN) (t/press! s sdl/K-DOWN) (t/press! s sdl/K-DOWN)
      (is (= 4 (t/caret s)))
      (t/press! s sdl/K-BACKSPACE)
      (is (= "one" (t/text s)))
      (is (= [[0 "in" [[0 "deeptwo"]]]] (sections s)))
      (is (= 4 (section-caret s)) "the caret where they joined"))))

(deftest shift-return-leaves-without-touching-what-is-in-them
  (testing "lists, in insert mode: only a line is added, and it can be undone"
    (with-session [s :mode :normal]
      (auk! s {:content ["one" {:type :list :content [{:text "a"} {:type :list :content [{:text ""}
                                {:type :list :content [{:text "deep"}]}]}]} "two"]})
      (press-i! s)
      (t/press! s sdl/K-DOWN)
      (t/press! s sdl/K-DOWN)
      (is (= [0 0] (insets/path (t/app s))))
      (let [before (sections s)]
        (t/press! s sdl/K-RETURN sdl/KMOD-SHIFT)
        (is (= "one\n\ntwo" (t/text s)) "a line after the top list")
        (is (nil? (insets/path (t/app s))))
        (is (= 4 (t/caret s)))
        (is (= (map rest before) (map rest (sections s))) "the lists are all there")
        (t/press! s sdl/K-ESCAPE)
        (t/type! s "u")
        (is (= "one\ntwo" (t/text s)) "and the line is in the history"))))
  (testing "in normal mode, and then in insert mode"
    (with-session [s :mode :normal]
      (auk! s {:content ["one" {:type :list :content [{:text "a"}]} "two"]})
      (t/press! s sdl/K-DOWN)
      (t/press! s sdl/K-RETURN sdl/KMOD-SHIFT)
      (is (= "one\n\ntwo" (t/text s)))
      (is (= :insert (:mode (t/app s))))))
  (testing "sections: after the innermost, in the text that holds it"
    (with-session [s :mode :normal]
      (auk! s {:content ["one" (ref 1) "two"]
               :sections [{:id 1 :content ["a" (ref 2) "b"]} {:id 2 :content ["x"]}]})
      (t/press! s sdl/K-DOWN)
      (t/press! s sdl/K-DOWN)
      (is (= [0 0] (insets/path (t/app s))))
      (t/press! s sdl/K-RETURN sdl/KMOD-SHIFT)
      (is (= [0] (insets/path (t/app s))) "in the outer section, not the buffer")
      (is (= "a\n\nb" (in-section s)) "a new line after the inner one"))))

(deftest k-deletes-a-mixed-selection
  (with-session [s :mode :normal]
    (auk! s {:content ["one" (ref 1) "two"] :sections [{:id 1 :content ["inner"]}]})
    (t/press! s sdl/K-A cmd)
    ;; the key, then its text, as SDL reports them
    (t/send! s {:type :key :key 0x6b :mod 0} {:type :text :text "k"})
    (is (= "" (t/text s)))
    (is (= [] (sections s)) "no confirmation, and the section that was all of it goes")
    (is (= :normal (:mode (t/app s))))))

(deftest up-and-down-cross-sections-in-sections
  (with-session [s :mode :normal]
    (auk! s {:content ["one" (ref 1) "two"]
             :sections [{:id 1 :content ["a" (ref 2) "b"]} {:id 2 :content ["x"]}]})
    (let [down! #(do (t/press! s sdl/K-DOWN) (insets/path (t/app s)))]
      (is (= [[0] [0 0] [0]] [(down!) (down!) (down!)])
          "into the outer one, the inner one, and out to the outer one")
      (is (= 2 (section-caret s)) "on its line below the inner one")
      (is (= [nil nil] [(down!) (down!)]) "then out of it"))
    (t/press! s sdl/K-UP)
    (t/press! s sdl/K-UP)
    (is (= [0 0] (insets/path (t/app s))) "and back up into the inner one")))

(deftest clicks-on-sections
  (with-session [s :mode :normal]
    (auk! s {:content ["one" (ref 1) "two"] :sections [{:id 1 :content ["inside"]}]})
    (let [m 48.0
          [tx ty _ th] (:text (section-place s 0))]
      (t/click! s (+ m tx 2) (+ m ty (quot th 2)))
      (is (= "inside" (in-section s)) "a click in a section puts the caret there")
      (is (zero? (section-caret s)))
      (t/click! s (+ m 200) (+ m ty (quot th 2)))
      (is (= 6 (section-caret s)) "where it is")
      (t/click! s (+ m 2) (+ m 2))
      (is (nil? (in-section s)) "and one in the text takes it out")
      (is (zero? (t/caret s))))
    (testing "folding the section the caret is in leaves it over the section"
      (t/press! s sdl/K-DOWN)
      (let [[x y _ h] (:header (section-place s 0))]
        (t/click! s (+ 48.0 x 10) (+ 48.0 y (quot h 2))))
      (is (= [0] (insets/path (t/app s))))
      (is (insets/over (t/app s)))
      (is (not (:modified? (t/app s))) "folding changes nothing in the file")
      (let [[x y _ h] (:header (section-place s 0))]
        (t/click! s (+ 48.0 x 10) (+ 48.0 y (quot h 2))))
      (is (= "inside" (in-section s)) "and unfolding it puts the caret back inside")
      (is (nil? (insets/over (t/app s)))))))

(deftest clicks-on-sections-in-sections
  (with-session [s :mode :normal]
    (auk! s {:content ["one" (ref 1)] :sections [{:id 1 :content ["a" (ref 2)]} {:id 2 :content ["x"]}]})
    (let [m 48.0
          [ox oy] (:text (section-place s 0))
          inner (first (:block-places (get-in (t/app s) [:insets 0])))
          [tx ty _ th] (:text inner)
          [hx hy _ hh] (:header inner)]
      (t/click! s (+ m ox tx 2) (+ m oy ty (quot th 2)))
      (is (= [0 0] (insets/path (t/app s))) "a click in the inner one puts the caret there")
      (t/click! s (+ m ox hx 10) (+ m oy hy (quot hh 2)))
      (is (get-in (t/app s) [:insets 0 :insets 0 :collapsed?]) "its header folds it")
      (is (= [0 0] (insets/path (t/app s))) "and leaves the caret over it")
      (is (insets/over (t/app s))))))

(deftest cmd-shift-k-deletes-the-section-once-asked
  (with-session [s :mode :normal]
    (auk! s {:content ["one" (ref 1)] :sections [{:id 1 :content ["doomed"]}]})
    (t/press! s sdl/K-K (bit-or cmd sdl/KMOD-SHIFT))
    (is (= "The caret is not in a section or list, or over a rule" (:message (t/app s))))
    (t/press! s sdl/K-DOWN)
    (t/press! s sdl/K-K (bit-or cmd sdl/KMOD-SHIFT))
    (is (= "Delete this section? (y/n)" (get-in (t/app s) [:confirm :prompt])))
    (t/press! s sdl/K-N)
    (is (:confirm (t/app s)) "a key waits for the answer")
    (t/type! s "n")
    (is (= "Cancelled" (:message (t/app s))))
    (is (= 1 (count (sections s))))
    (t/press! s sdl/K-K (bit-or cmd sdl/KMOD-SHIFT))
    (t/press! s sdl/K-ESCAPE)
    (is (= "Cancelled" (:message (t/app s))) "escape answers no")
    (t/press! s sdl/K-K (bit-or cmd sdl/KMOD-SHIFT))
    (t/type! s "y")
    (is (empty? (sections s)))
    (is (nil? (in-section s)) "the caret is back in the text")
    (is (= "Deleted the section" (:message (t/app s))))
    (is (:modified? (t/app s)))))

(deftest cmd-shift-k-in-a-section-in-a-section-deletes-just-that
  (with-session [s :mode :normal]
    (auk! s {:content ["one" (ref 1)] :sections [{:id 1 :content ["a" (ref 2)]} {:id 2 :content ["x"]}]})
    (t/press! s sdl/K-DOWN)
    (t/press! s sdl/K-DOWN)
    (t/press! s sdl/K-K (bit-or cmd sdl/KMOD-SHIFT))
    (t/type! s "y")
    (is (= [[0 "a"]] (sections s)))
    (is (= [0] (insets/path (t/app s))) "the caret is back in the outer one")))

(deftest a-section-has-its-own-undo
  (with-session [s :mode :normal]
    (auk! s {:content ["one"]})
    (t/type! s "A")
    (t/type! s " more")
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-S cmd)
    (t/type! s "i")
    (t/type! s "noted")
    (t/press! s sdl/K-ESCAPE)
    (t/type! s "u")
    (is (= "" (in-section s)) "undo in a section undoes its typing")
    (is (= "one more\n" (t/text s)) "and not the text's")
    (t/press! s sdl/K-UP)
    (t/type! s "u")
    (is (= "one more" (t/text s)) "the new line after it is the text's to undo")
    (is (= [[0 ""]] (sections s)) "which leaves the section")))

(deftest auk-help-comes-first-in-auk-mode
  (with-session [s :mode :normal]
    (let [titles #(keep (fn [[kind title]] (when (= kind :title) title)) (#'help-window/items (t/app s)))]
      (is (= "Normal mode" (first (titles))))
      (auk! s {:content []})
      (is (= ["Auk mode" "Normal mode"] (take 2 (titles)))))))

(deftest insets-need-a-mode-to-write-them
  (with-session [s :mode :normal]
    (auk! s {:content ["one" (ref 1)] :sections [{:id 1 :content ["x"]}]})
    (t/command! s "mode text")
    (t/command! s "save")
    (t/send! s {:type :save-chosen :path "/notes/n.txt"})
    (is (= "Can't write n.txt: only a mode can write its insets, and text mode can't"
           (:message (t/app s))))
    (is (nil? (get-in @s [:files "/notes/n.txt"])))))

(deftest reverting-reads-the-sections-again
  (with-session [s :mode :normal]
    (swap! s assoc-in [:files "/notes/n.auk"]
           (pr-str {:content ["one" (ref 1)] :sections [{:id 1 :content ["kept" (ref 2)]} {:id 2 :content ["in"]}]}))
    (auk! s {:content ["one"]})
    (t/press! s sdl/K-S cmd)
    (t/command! s "revert")
    (is (= [[0 "kept" [[0 "in"]]]] (sections s)))
    (is (nil? (in-section s)))
    (is (not (:modified? (t/app s))))))

(deftest each-buffer-keeps-its-sections
  (with-session [s :mode :normal]
    (auk! s {:content ["one" (ref 1)] :sections [{:id 1 :content ["mine"]}]})
    (t/press! s sdl/K-DOWN)
    (open! s "/notes/other.txt" "plain")
    (is (empty? (sections s)))
    (is (nil? (in-section s)))
    (auk! s {:content []})
    (is (= "mine" (in-section s)) "the caret still in its section")))

(deftest a-mode-binds-key-chords
  (with-session [s :mode :normal
                 :mode-sources [["chord.clj"
                                 "(ns chord (:require [hoatzin.mode :as mode]))
                                  {:name \"chord\" :extensions [\"chord\"]
                                   :normal {\"shift+cmd+j\" (fn [app _] (mode/message app \"chord\"))}}"]]]
    (open! s "/x.chord" "")
    (t/press! s (int \j) (bit-or cmd sdl/KMOD-SHIFT))
    (is (= "chord" (:message (t/app s))) "its modifiers in any order")
    (t/press! s sdl/K-LEFT)
    (t/press! s (int \j) cmd)
    (is (nil? (:message (t/app s))) "and only with them all")))

;; ---------------------------------------------------------------- folding, renaming, lists

(def ^:private ctrl sdl/KMOD-CTRL)

(defn- key! [s c mod] (t/press! s (int c) mod))

(deftest space-folds-and-unfolds-the-section
  (with-session [s :mode :normal]
    (auk! s {:content ["one" (ref 1) "two"] :sections [{:id 1 :content ["a" "b"]}]})
    (t/type! s " ")
    (is (nil? (insets/path (t/app s))) "outside a section, space does nothing")
    (t/press! s sdl/K-DOWN)
    (t/press! s sdl/K-DOWN)
    (t/type! s " ")
    (is (get-in (t/app s) [:insets 0 :collapsed?]) "inside one, space folds it")
    (is (insets/over (t/app s)) "leaving the caret over it")
    (t/type! s " ")
    (is (not (get-in (t/app s) [:insets 0 :collapsed?])) "over it, space unfolds it")
    (is (= "a\nb" (in-section s)) "and puts the caret inside")
    (is (= 2 (section-caret s)) "where it was")))

(deftest over-a-folded-section-the-text-is-left-alone
  (with-session [s :mode :normal]
    (auk! s {:content ["one" (ref 1) "two"] :sections [{:id 1 :content ["a"]}]})
    (t/press! s sdl/K-DOWN)
    (t/type! s " ")
    (t/type! s "x")
    (t/type! s "i")
    (t/type! s "zz")
    (is (= :normal (:mode (t/app s))))
    (is (= "one\ntwo" (t/text s)))
    (is (= [{:after 0 :text "a" :insets []}] (insets/snapshot (t/app s))))
    (t/type! s ":")
    (is (= :command (:mode (t/app s))) "but the command line opens")
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-K (bit-or cmd sdl/KMOD-SHIFT))
    (t/type! s "y")
    (is (empty? (sections s)) "and cmd+shift+k deletes it")))

(deftest cmd-r-renames-the-section
  (with-session [s :mode :normal]
    (auk! s {:content ["one" (ref 1)] :sections [{:id 1 :content ["a"]}]})
    (t/press! s sdl/K-DOWN)
    (key! s \r cmd)
    (is (= [0] (:renaming (t/app s))))
    (t/type! s "Habitatx")
    (t/press! s sdl/K-BACKSPACE)
    (is (= "Habitat" (insets/renaming (t/app s))))
    (is (= "a" (str (get-in (t/app s) [:insets 0 :doc :text]))) "not typed into the text")
    (t/press! s sdl/K-RETURN)
    (is (nil? (:renaming (t/app s))))
    (is (= "Habitat" (get-in (t/app s) [:insets 0 :title])))
    (is (= "a" (in-section s)) "the text is as it was")
    (is (some #(= "Habitat" (get-in % [:node :text])) (:placed (section-place s 0))) "the header says it")
    (t/command! s "w")
    (is (str/includes? (get-in @s [:files "/notes/n.auk"]) "{:id 1 :title \"Habitat\" :content [\"a\"]}"))
    (testing "up or down keep it too, and escape gives it up"
      (key! s \r cmd)
      (t/type! s "s")
      (t/press! s sdl/K-UP)
      (is (= "Habitats" (get-in (t/app s) [:insets 0 :title])))
      (key! s \r cmd)
      (t/type! s "!!")
      (t/press! s sdl/K-ESCAPE)
      (is (= "Habitats" (get-in (t/app s) [:insets 0 :title]))))
    (testing "over a folded one, and to nothing, which takes the title away"
      (t/type! s " ")
      (key! s \r cmd)
      (dotimes [_ 8] (t/press! s sdl/K-BACKSPACE))
      (t/press! s sdl/K-DOWN)
      (is (nil? (get-in (t/app s) [:insets 0 :title]))))
    (testing "not outside a section"
      (t/press! s sdl/K-UP)
      (key! s \r cmd)
      (is (nil? (:renaming (t/app s))))
      (is (= "The caret is not in a section" (:message (t/app s)))))))

(deftest l-starts-a-list
  (with-session [s :mode :normal]
    (auk! s {:content ["one" "two"]})
    (t/type! s "l")
    (is (= [[0 ""]] (sections s)) "below the line")
    (is (= :list (:kind (insets/innermost (t/app s)))))
    (is (= "one\ntwo" (t/text s)) "with no new line after it: there is a line there already")
    (is (= :normal (:mode (t/app s))) "still in normal mode")
    (t/type! s "i")
    (t/type! s "eggs")
    (t/press! s sdl/K-RETURN)
    (t/type! s "leaves")
    (t/press! s sdl/K-ESCAPE)
    (is (= "eggs\nleaves" (in-section s)) "each line an item")
    (t/command! s "w")
    (is (= (str "{:content\n [\"one\"\n  {:type :list :content [{:text \"eggs\"} {:text \"leaves\"}]}\n"
                "  \"two\"]}\n")
           (get-in @s [:files "/notes/n.auk"])))
    (testing "reads back as it was"
      (open! s "/notes/copy.auk" (get-in @s [:files "/notes/n.auk"]))
      (is (= [{:after 0 :text "eggs\nleaves" :insets [] :kind :list}] (insets/snapshot (t/app s)))))))

(deftest cmd-l-inserts-a-list-in-insert-mode
  (with-session [s :mode :normal]
    (auk! s {:content ["one"]})
    (t/type! s "A")
    (key! s \l cmd)
    (is (= :list (:kind (insets/innermost (t/app s)))))
    (is (= :insert (:mode (t/app s))) "still in insert mode")
    (t/type! s "first")
    (is (= "first" (in-section s)))
    (t/press! s sdl/K-ESCAPE)
    (key! s \l cmd)
    (is (= 1 (count (sections s))) "not in normal mode")))

(deftest checklists-tick-and-keep-their-ticks
  (with-session [s :mode :normal]
    (auk! s {:content ["one"]})
    (key! s \l ctrl)
    (is (= :checklist (:kind (insets/innermost (t/app s)))))
    (t/type! s "i")
    (t/type! s "eggs\nleaves")
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-UP)
    (t/type! s "t")
    (is (= [true false] (:checked (first (insets/snapshot (t/app s))))) "t ticks the item the caret is in")
    (t/type! s "t")
    (is (= [false false] (:checked (first (insets/snapshot (t/app s))))) "and unticks it")
    (t/type! s "t")
    (testing "ticks follow their items as the list is edited"
      (t/type! s "O")
      (t/type! s "nest")
      (t/press! s sdl/K-ESCAPE)
      (is (= "nest\neggs\nleaves" (in-section s)))
      (is (= [false true false] (:checked (first (insets/snapshot (t/app s))))))
      (t/press! s sdl/K-DOWN)
      (t/type! s "A")
      (t/press! s sdl/K-RETURN)
      (t/type! s "bark")
      (t/press! s sdl/K-ESCAPE)
      (is (= [false true false false] (:checked (first (insets/snapshot (t/app s)))))))
    (t/command! s "w")
    (is (str/includes? (get-in @s [:files "/notes/n.auk"])
                       (str "{:type :checklist :content [{:text \"nest\" :checked? false} "
                            "{:text \"eggs\" :checked? true} {:text \"bark\" :checked? false} "
                            "{:text \"leaves\" :checked? false}]}")))
    (open! s "/notes/copy.auk" (get-in @s [:files "/notes/n.auk"]))
    (is (= [false true false false] (:checked (first (insets/snapshot (t/app s))))) "and read back")
    (testing "t outside a checklist"
      (t/type! s "t")
      (is (= "The caret is not in a checklist" (:message (t/app s)))))))

(deftest cmd-ctrl-l-inserts-a-checklist-in-insert-mode
  (with-session [s :mode :normal]
    (auk! s {:content ["one"]})
    (t/type! s "A")
    (key! s \l (bit-or cmd ctrl))
    (is (= :checklist (:kind (insets/innermost (t/app s)))))
    (is (= :insert (:mode (t/app s))))))

(deftest lists-hold-no-sections
  (with-session [s :mode :normal]
    (auk! s {:content ["one" (ref 1)] :sections [{:id 1 :content ["a"]}]})
    (t/press! s sdl/K-DOWN)
    (t/type! s "l")
    (is (= [0 0] (insets/path (t/app s))) "a list in a section")
    (t/press! s sdl/K-S cmd)
    (is (= [[0 "a\n" [[0 "" ] [0 ""]]]] (sections s)) "cmd+s in it adds a section after it, in the section")
    (is (= [:list :section] (map #(:kind % :section) (:insets (first (insets/snapshot (t/app s))))))
        "in that order")
    (t/press! s sdl/K-UP)
    (is (= :list (:kind (insets/innermost (t/app s)))))
    (t/type! s " ")
    (is (insets/over (t/app s)) "space in a list folds the section it is in")
    (is (= [0] (insets/path (t/app s))))))

;; ---------------------------------------------------------------- k, nested lists, tab, return

(deftest k-deletes-forwards-in-normal-mode
  (with-session [s :mode :normal]
    (auk! s {:content ["one two" (ref 1) "three"] :sections [{:id 1 :content ["in"]}]})
    (t/type! s "k")
    (is (= "ne two\nthree" (t/text s)) "the character after the caret")
    (is (= :normal (:mode (t/app s))))
    (t/type! s "w")
    (t/type! s "k")
    (is (= " two\nthree" (t/text s)) "or the selection")
    (t/press! s sdl/K-END)
    (t/type! s "k")
    (is (= " two\nthree" (t/text s)) "but not past the end of a line a section is below")
    (is (= [[0 "in"]] (sections s)))
    (t/press! s sdl/K-DOWN)
    (t/press! s sdl/K-A sdl/KMOD-CTRL)
    (t/type! s "k")
    (is (= "n" (in-section s)) "in a section, its own text")
    (t/type! s "u")
    (is (= "in" (in-section s)) "and undo puts it back")
    (testing "at the end of the text, nothing"
      (t/press! s sdl/K-DOWN)
      (t/press! s sdl/K-END)
      (t/type! s "k")
      (is (= " two\nthree" (t/text s))))))

(deftest a-list-key-in-a-list-starts-a-sublist
  (with-session [s :mode :normal]
    (auk! s {:content ["one" {:type :list :content [{:text "a"} {:text "b"}]}]})
    (t/press! s sdl/K-DOWN)
    (t/type! s "l")
    (is (= 2 (count (insets/path (t/app s)))) "in the list")
    (is (= [[0 "a\nb" [[0 ""]]]] (sections s)) "below the item the caret is in, with no new item")
    (t/type! s "i")
    (t/type! s "a1")
    (t/press! s sdl/K-ESCAPE)
    (key! s \l ctrl)
    (is (= [[0 "a\nb" [[0 "a1" [[0 ""]]]]]] (sections s)) "to any depth, of either kind")
    (t/command! s "w")
    (is (str/includes? (get-in @s [:files "/notes/n.auk"])
                       (str "{:type :list :content [{:text \"a\"} {:type :list :content [{:text \"a1\"} "
                            "{:type :checklist :content [{:text \"\" :checked? false}]}]} {:text \"b\"}]}")))
    (open! s "/notes/copy.auk" (get-in @s [:files "/notes/n.auk"]))
    (is (= [[0 "a\nb" [[0 "a1" [[0 ""]]]]]] (sections s)) "and reads back")))

(defn- list! [s items] (auk! s {:content ["one" {:type :list :content items}]}))

(deftest tab-cycles-a-list-items-indent
  (with-session [s :mode :normal]
    (list! s [{:text "a"} {:text "b"} {:text "c"}])
    (t/press! s sdl/K-DOWN)
    (t/press! s sdl/K-DOWN)
    (t/press! s sdl/K-TAB)
    (is (= [[0 "a\nc" [[0 "b"]]]] (sections s)) "into a sublist of the item before it")
    (is (= "b" (in-section s)) "the caret with it")
    (t/press! s sdl/K-TAB)
    (is (= [[0 "a\nb\nc"]] (sections s)) "with no list above, back where it was")
    (is (= "a\nb\nc" (in-section s)))
    (is (= 2 (section-caret s)))
    (t/press! s sdl/K-TAB)
    (is (= [[0 "a\nc" [[0 "b"]]]] (sections s)) "and round again")
    (testing "the first item has no item to go under"
      (t/press! s sdl/K-TAB)
      (t/press! s sdl/K-UP)
      (t/press! s sdl/K-TAB)
      (is (= [[0 "a\nb\nc"]] (sections s))))))

(deftest tab-takes-a-sublist-item-out-and-back
  (with-session [s :mode :normal]
    (list! s [{:text "a"} {:type :list :content [{:text "x"} {:text "y"} {:text "z"}]} {:text "b"}])
    (t/press! s sdl/K-DOWN)
    (t/press! s sdl/K-DOWN)
    (t/press! s sdl/K-DOWN)
    (is (= "x\ny\nz" (in-section s)))
    (t/type! s "A")
    (t/press! s sdl/K-TAB)
    (is (= [[0 "a\nb" [[0 "x\nz" [[0 "y"]]]]]] (sections s)) "in, under x")
    (t/press! s sdl/K-TAB)
    (is (= [[0 "a\ny\nb" [[0 "x"] [1 "z"]]]] (sections s))
        "out, to the list holding its list, the items after it its sublist")
    (is (= "a\ny\nb" (in-section s)))
    (is (= 3 (section-caret s)) "the caret where it was along it")
    (t/press! s sdl/K-TAB)
    (is (= [[0 "a\nb" [[0 "x\ny\nz"]]]] (sections s)) "and back")
    (is (= :insert (:mode (t/app s))) "in insert mode too")))

(deftest tab-on-a-sublists-first-item-takes-it-out
  ;; it can't go in, having no item before it, so it goes out; its list,
  ;; left with nothing, goes, which once left the caret pointing at it
  (with-session [s :mode :normal]
    (list! s [{:text "a"} {:type :list :content [{:text "x"} {:text "y"}]} {:text "b"}])
    (t/press! s sdl/K-DOWN)
    (t/press! s sdl/K-DOWN)
    (is (= "x\ny" (in-section s)))
    (t/press! s sdl/K-TAB)
    (is (= [[0 "a\nx\nb" [[1 "y"]]]] (sections s)) "out, the item after it its sublist")
    (is (= "a\nx\nb" (in-section s)))
    (is (= 2 (section-caret s)))
    (t/render! s)
    (t/press! s sdl/K-TAB)
    (is (= [[0 "a\nb" [[0 "x\ny"]]]] (sections s)) "and back")
    (t/render! s)))

(deftest a-click-on-a-box-ticks-it
  (with-session [s :mode :normal]
    (auk! s {:content ["one" {:type :checklist :content [{:text "a"} {:text "b"}]}]})
    (let [m 48.0
          lh (layout/line-height (:layout (t/app s)))
          [tx ty] (:text (section-place s 0))]
      (t/click! s (+ m tx -10) (+ m ty lh (quot lh 2)))
      (is (= [false true] (:checked (first (insets/snapshot (t/app s))))) "the item beside it")
      (is (nil? (insets/path (t/app s))) "leaving the caret where it was")
      (is (:modified? (t/app s)))
      (t/click! s (+ m tx -10) (+ m ty lh (quot lh 2)))
      (is (= [false false] (:checked (first (insets/snapshot (t/app s)))))))))

(deftest return-twice-leaves-the-list
  (with-session [s :mode :normal]
    (auk! s {:content ["one" {:type :list :content [{:text "a"}]} "two"]})
    (t/press! s sdl/K-DOWN)
    (t/type! s "A")
    (t/press! s sdl/K-RETURN)
    (is (= "a\n" (in-section s)) "the first makes an item")
    (t/press! s sdl/K-RETURN)
    (is (nil? (insets/path (t/app s))) "the second leaves the list")
    (is (= [[0 "a"]] (sections s)) "taking the empty item away")
    (is (= "one\n\ntwo" (t/text s)) "for a new line after it")
    (is (= 4 (t/caret s)))
    (is (= :insert (:mode (t/app s))))
    (testing "from a sublist, one level"
      (t/press! s sdl/K-ESCAPE)
      (open! s "/notes/sub.auk" (pr-str {:content ["one" {:type :list :content [{:text "a"} {:type :list :content [{:text "x"}]}]}]}))
      (t/press! s sdl/K-DOWN)
      (t/press! s sdl/K-DOWN)
      (t/type! s "A")
      (t/press! s sdl/K-RETURN)
      (t/press! s sdl/K-RETURN)
      (is (= 1 (count (insets/path (t/app s)))) "out to the list holding it")
      (is (= [[0 "a\n" [[0 "x"]]]] (sections s)) "on a new item after it"))
    (testing "a list of an empty item goes"
      (t/press! s sdl/K-ESCAPE)
      (open! s "/notes/e.auk" (pr-str {:content ["one" {:type :list :content [{:text ""}]}]}))
      (t/press! s sdl/K-DOWN)
      (t/type! s "i")
      (t/press! s sdl/K-RETURN)
      (is (empty? (sections s)))
      (is (= "one\n" (t/text s))))))

(deftest shift-return-leaves-every-list
  (with-session [s :mode :normal]
    (list! s [{:text "a"} {:type :list :content [{:text "x"}]}])
    (t/press! s sdl/K-DOWN)
    (t/press! s sdl/K-DOWN)
    (t/type! s "A")
    (t/press! s sdl/K-RETURN sdl/KMOD-SHIFT)
    (is (nil? (insets/path (t/app s))))
    (is (= [[0 "a" [[0 "x"]]]] (sections s)) "an item with text stays")
    (is (= "one\n" (t/text s)) "and a new line goes after the outermost")
    (is (= 4 (t/caret s)))))

(deftest cmd-k-deletes-the-line
  (with-session [s :mode :normal]
    (t/type! s "i")
    (t/type! s "one\ntwo\nthree")
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-UP)
    (t/press! s sdl/K-K cmd)
    (is (= "one\nthree" (t/text s)) "the line the caret is in, and its newline")
    (is (= 4 (t/caret s)) "the caret at the start of the next")
    (t/press! s sdl/K-K cmd)
    (is (= "one" (t/text s)))
    (is (= 0 (t/caret s)) "or, at the end, of the one before")
    (t/press! s sdl/K-K cmd)
    (is (= "" (t/text s)) "a lone line is emptied")
    (t/type! s "u")
    (is (= "one" (t/text s)) "undo puts it back, a line at a time")
    (testing "only in normal mode"
      (t/type! s "i")
      (t/press! s sdl/K-K cmd)
      (is (= "one" (t/text s))))))

(deftest cmd-k-leaves-the-sections-below-the-line
  (with-session [s :mode :normal]
    (auk! s {:content ["one" (ref 1) "two" "three"] :sections [{:id 1 :content ["a" "b"]}]})
    (t/press! s sdl/K-K cmd)
    (is (= "two\nthree" (t/text s)))
    (is (= [[-1 "a\nb"]] (sections s)) "a section below the first line goes above the new first")
    (t/press! s sdl/K-UP)
    (is (= "a\nb" (in-section s)))
    (t/press! s sdl/K-K cmd)
    (is (= "a" (in-section s)) "in a section, its line")
    (is (= [[-1 "a"]] (sections s)) "and the section stays")
    (testing "and in a list, its item"
      (open! s "/notes/l.auk" (pr-str {:content ["one" {:type :checklist :content [{:text "x" :checked? true}
                                                                                 {:text "y"}]}]}))
      (t/press! s sdl/K-DOWN)
      (t/press! s sdl/K-K cmd)
      (is (= [{:after 0 :text "y" :insets [] :kind :checklist :checked [false]}] (insets/snapshot (t/app s)))))))

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
    (is (= [["buffers"] ["cd"] ["close"] ["minor"] ["mode"] ["new"] ["open"] ["quit"] ["revert"]
            ["save"] ["settings"] ["write"]]
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
    (is (nil? (hints s)) "gone with the command line")
    (t/type! s ":mode a")
    (is (= [["auk"]] (hints s)) "once the command has an argument, the arguments it could be")
    (t/press! s sdl/K-ESCAPE)
    (t/type! s ":open a")
    (is (nil? (hints s)) "none for an argument that could be anything")
    (t/press! s sdl/K-ESCAPE))
  (testing "in two rows, down then across, when they don't fit on one"
    (with-session [s :mode nil :width 220]
      (t/type! s ":")
      (is (= [["buffers" "cd"] ["close" "minor"]] (hints s)))))
  (testing "no more than two rows: the columns that don't fit are left out"
    (is (= [["a" "b"] ["c" "d"]] (#'hints/columns ["a" "b" "c" "d" "e" "f"] 10 5 25)))
    (is (= [["a"] ["b"] ["c"]] (#'hints/columns ["a" "b" "c"] 10 5 40)))
    (is (= [["a" "b"]] (#'hints/columns ["a" "b" "c"] 10 5 1)) "always one column")))

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
            :settings/theme         "Dark 3"
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
    (click-field! s :settings/line-height)
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
      (is (= :settings/theme (:focus (t/app s))))
      (t/press! s sdl/K-TAB)
      (is (= :settings/editor-family (:focus (t/app s))) "wrapping around")
      (t/press! s sdl/K-TAB sdl/KMOD-SHIFT)
      (is (= :settings/theme (:focus (t/app s))))
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

(deftest choosing-a-theme
  (with-session [s :mode :normal]
    (t/command! s "settings")
    (is (= (theme/colours "Dark 3") (select-keys (t/app s) (keys (theme/colours "Dark 3")))))
    (click-field! s :settings/theme)
    (is (= 5 (:active (list-of s))) "on the theme in use")
    (t/press! s sdl/K-HOME)
    (t/press! s sdl/K-RETURN)
    (is (= "Light 1" (get-in (t/app s) [:settings :theme])))
    (is (= "Light 1" (get-in @s [:saved-settings :theme])) "saving it")
    (t/render! s)
    (is (= [255 255 255] (:background (t/app s))) "its colours apply at once")
    (is (= [0 0 0] (:foreground (t/app s))))
    (is (= [90 125 124] (:status-background (t/app s))))))

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
    (t/press! s sdl/K-A sdl/KMOD-CTRL)
    (is (= 0 (t/caret s)))
    (t/press! s sdl/K-M cmd)
    (is (= 2 (t/caret s)))
    (t/press! s sdl/K-E sdl/KMOD-CTRL)
    (is (= 9 (t/caret s)))
    (t/press! s sdl/K-M cmd)
    (t/type! s "w")
    (t/type! s "c")
    (is (= "one" (:clipboard @s)) "c copies the word")
    (is (nil? (t/selected s)) "and leaves nothing selected")
    (is (= 5 (t/caret s)) "the caret where it was")
    (t/set-clipboard! s "x")
    (t/press! s sdl/K-M cmd)
    (t/type! s "w")
    (t/type! s "x")
    (is (= "   two" (t/text s)) "x cut the word")
    (is (nil? (t/selected s)))
    (t/type! s "p")
    (is (= "  one two" (t/text s)) "p pastes it back")
    (is (= :normal (:mode (t/app s))))))

(deftest line-keys-in-both-modes
  (with-session [s]
    (t/type! s "  one two")
    (t/press! s sdl/K-A sdl/KMOD-CTRL)
    (is (= 0 (t/caret s)))
    (t/press! s sdl/K-E sdl/KMOD-CTRL)
    (is (= 9 (t/caret s)))
    (t/press! s sdl/K-M cmd)
    (is (= 2 (t/caret s)) "cmd+m: the first non-blank")
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-A sdl/KMOD-CTRL)
    (is (= 0 (t/caret s)) "and the same in normal mode")
    (t/press! s sdl/K-E sdl/KMOD-CTRL)
    (is (= 9 (t/caret s)))
    (t/press! s sdl/K-M (bit-or cmd sdl/KMOD-SHIFT))
    (is (= "one two" (t/selected s)) "shift extends")))

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

;; ---------------------------------------------------------------- variants

(defn- variants
  "The current buffer's variants, as the file beside it would keep them."
  [s]
  (let [a (t/app s)
        marks (get-in a [:doc :marks])]
    (->> (:variants a)
         (keep (fn [[id {:keys [options selected]}]]
                 (let [st (get marks [:variant id :start]) e (get marks [:variant id :end])]
                   (when (< st e) {:start st :end e :options options :selected selected}))))
         (sort-by :start)
         vec)))

(defn- variant-file! [s path text]
  (t/send! s {:type :opened :path path :text text})
  (t/press! s sdl/K-ESCAPE))

(defn- select-word-at!
  "Put the caret at `pos` and select the word there, in normal mode."
  [s pos]
  (t/press! s sdl/K-UP cmd)
  (dotimes [_ pos] (t/press! s sdl/K-RIGHT))
  (t/type! s "w"))

(deftest v-adds-a-variant-of-the-selection
  (with-session [s]
    (variant-file! s "/birds/a.txt" "a testing word")
    (select-word-at! s 2)
    (is (= "testing" (t/selected s)))
    (t/type! s "v")
    (is (= "a  word" (t/text s)) "the selection goes")
    (is (= 2 (t/caret s)))
    (is (= :insert (:mode (t/app s))))
    (let [[_ _ w h] (app/caret-rect (t/app s))]
      (is (< h w) "the caret is an underline"))
    (t/type! s "tested")
    (is (= "a tested word" (t/text s)))
    (t/press! s sdl/K-RETURN)
    (is (= "a tested word" (t/text s)) "return keeps it, and isn't typed")
    (is (= :normal (:mode (t/app s))))
    (is (nil? (:caret-shape (t/app s))))
    (is (= 2 (t/caret s)) "the caret goes to its start")
    (is (= "Variant 2 of 2" (:message (t/app s))))
    (is (= [{:start 2 :end 8 :options ["testing" "tested"] :selected 1}] (variants s)))))

(deftest n-shows-the-next-wording
  (with-session [s]
    (variant-file! s "/birds/a.txt" "a testing word")
    (select-word-at! s 2)
    (t/type! s "vtested")
    (t/press! s sdl/K-RETURN)
    (t/type! s "n")
    (is (= "a testing word" (t/text s)))
    (is (= "Variant 1 of 2" (:message (t/app s))))
    (is (= [{:start 2 :end 9 :options ["testing" "tested"] :selected 0}] (variants s)))
    (t/press! s sdl/K-RIGHT)
    (t/press! s sdl/K-RIGHT)
    (t/type! s "n")
    (is (= "a tested word" (t/text s)) "anywhere over it")
    (is (= 2 (t/caret s)))
    (t/press! s sdl/K-LEFT)
    (t/type! s "n")
    (is (= "a tested word" (t/text s)) "and not off it")
    (testing "undo puts the wording back, and the variant stays"
      (t/press! s sdl/K-RIGHT)
      (t/type! s "n")
      (is (= "a testing word" (t/text s)))
      (t/type! s "u")
      (is (= "a tested word" (t/text s)))
      (is (= [{:start 2 :end 8 :options ["testing" "tested"] :selected 0}] (variants s)))
      (t/press! s sdl/K-LEFT)
      (t/type! s "n")
      (is (= "a testing word" (t/text s)) "the wording shown is the one it is")
      (is (= [{:start 2 :end 9 :options ["testing" "tested"] :selected 0}] (variants s))))))

(deftest escape-gives-up-the-variant
  (with-session [s]
    (variant-file! s "/birds/a.txt" "a testing word")
    (select-word-at! s 2)
    (t/type! s "vtes")
    (t/press! s sdl/K-ESCAPE)
    (is (= "a testing word" (t/text s)))
    (is (= "testing" (t/selected s)) "with the text selected again")
    (is (= :normal (:mode (t/app s))))
    (is (= [] (variants s)))))

(deftest a-variant-gets-more-wordings
  (with-session [s]
    (variant-file! s "/birds/a.txt" "a testing word")
    (select-word-at! s 2)
    (t/type! s "vtested")
    (t/press! s sdl/K-RETURN)
    (t/type! s "v")
    (is (= "a  word" (t/text s)) "over a variant, v takes it all")
    (t/type! s "tests")
    (t/press! s sdl/K-ESCAPE)
    (is (= "a tested word" (t/text s)))
    (is (= [{:start 2 :end 8 :options ["testing" "tested"] :selected 1}] (variants s))
        "giving up leaves it as it was")
    (t/type! s "vtests")
    (t/press! s sdl/K-RETURN)
    (is (= "Variant 3 of 3" (:message (t/app s))))
    (is (= [{:start 2 :end 7 :options ["testing" "tested" "tests"] :selected 2}] (variants s)))
    (t/type! s "n")
    (is (= "a testing word" (t/text s)) "round to the first")))

(deftest v-needs-text-of-its-own
  (with-session [s]
    (variant-file! s "/birds/a.txt" "a testing word")
    (t/type! s "v")
    (is (= "Select the text to add a variant of" (:message (t/app s))))
    (is (= :normal (:mode (t/app s))))
    (select-word-at! s 2)
    (t/type! s "vtested")
    (t/press! s sdl/K-RETURN)
    (t/press! s sdl/K-UP cmd)
    (t/type! s "ws")
    (t/type! s "v")
    (is (= "The selection overlaps a variant" (:message (t/app s))))
    (testing "an empty variant is none"
      (t/press! s sdl/K-ESCAPE)
      (select-word-at! s 9)
      (t/type! s "v")
      (t/press! s sdl/K-RETURN)
      (is (= "a tested word" (t/text s)))
      (is (= "A variant can't be empty" (:message (t/app s))))
      (is (= 1 (count (variants s)))))))

(deftest typing-a-variant-keeps-to-it
  (with-session [s]
    (variant-file! s "/birds/a.txt" "a testing word")
    (select-word-at! s 2)
    (t/type! s "vab")
    (t/press! s sdl/K-LEFT)
    (t/press! s sdl/K-LEFT)
    (t/press! s sdl/K-LEFT)
    (is (= 2 (t/caret s)) "not out past its start")
    (t/press! s sdl/K-BACKSPACE)
    (is (= "a ab word" (t/text s)))
    (t/press! s sdl/K-RIGHT)
    (t/press! s sdl/K-RIGHT)
    (t/press! s sdl/K-RIGHT)
    (is (= 4 (t/caret s)) "nor past its end")
    (t/press! s sdl/K-DELETE)
    (is (= "a ab word" (t/text s)))
    (t/press! s sdl/K-UP)
    (t/press! s sdl/K-TAB)
    (is (= 4 (t/caret s)) "other keys do nothing")
    (t/press! s sdl/K-BACKSPACE)
    (t/press! s sdl/K-RETURN)
    (is (= [{:start 2 :end 3 :options ["testing" "a"] :selected 1}] (variants s)))))

(deftest a-click-gives-up-the-variant
  (with-session [s]
    (variant-file! s "/birds/a.txt" "a testing word")
    (select-word-at! s 2)
    (t/type! s "vx")
    (t/click! s 300 200)
    (is (= "a testing word" (t/text s)))
    (is (= :normal (:mode (t/app s))))))

(deftest a-variant-edited-shows-what-is-there
  (with-session [s]
    (variant-file! s "/birds/a.txt" "a testing word")
    (select-word-at! s 2)
    (t/type! s "vtested")
    (t/press! s sdl/K-RETURN)
    (t/press! s sdl/K-RIGHT)
    (t/type! s "i")
    (t/type! s "o")
    (t/press! s sdl/K-ESCAPE)
    (is (= "a toested word" (t/text s)))
    (t/type! s "n")
    (is (= "a testing word" (t/text s)))
    (t/type! s "n")
    (is (= "a toested word" (t/text s)) "the edit is the wording it was")))

(deftest variants-are-kept-beside-the-file
  (with-session [s]
    (variant-file! s "/birds/a.txt" "a testing word\n")
    (is (= [["variants" "-birds-a.txt.edn"]] (:data-asked @s)) "asked for as the file is opened")
    (t/data-read! s "variants" "-birds-a.txt.edn")
    (is (= [] (variants s)) "there are none")
    (select-word-at! s 2)
    (t/type! s "vtested")
    (t/press! s sdl/K-RETURN)
    (is (= {} (:mode-data @s)) "not until the file is written")
    (t/type! s "n")
    (t/command! s "w")
    (is (= "a testing word\n" (get-in @s [:files "/birds/a.txt"])))
    (is (= {["variants" "-birds-a.txt.edn"]
            {:variants [{:start 2 :end 9 :options ["testing" "tested"] :selected 0}] :dims []}}
           (:mode-data @s)))
    (t/command! s "close")
    (t/send! s {:type :opened :path "/birds/a.txt" :text (get-in @s [:files "/birds/a.txt"])})
    (is (= [] (variants s)) "they come later")
    (t/data-read! s "variants" "-birds-a.txt.edn")
    (is (= [{:start 2 :end 9 :options ["testing" "tested"] :selected 0}] (variants s)))
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-RIGHT)
    (t/press! s sdl/K-RIGHT)
    (t/type! s "n")
    (is (= "a tested word\n" (t/text s)))
    (testing "read again, they are not had twice"
      (t/data-read! s "variants" "-birds-a.txt.edn")
      (is (= 1 (count (variants s)))))
    (testing "with none left, none are kept"
      (select-word-at! s 2)
      (t/type! s "x")
      (is (= "a  word\n" (t/text s)))
      (t/command! s "w")
      (is (= {} (:mode-data @s))))))

(deftest variants-read-are-of-the-text
  (with-session [s]
    (swap! s assoc :mode-data {["variants" "-birds-a.txt.edn"]
                               {:variants [{:start 2 :end 9 :options ["testing" "tested"] :selected 0}
                                           {:start 4 :end 6 :options ["st" "ST"] :selected 0}
                                           {:start 10 :end 14 :options ["verb" "word"] :selected 0}
                                           {:start 10 :end 99 :options ["x" "y"] :selected 0}]}})
    (variant-file! s "/birds/a.txt" "a testing word")
    (t/command! s "new")
    (t/data-read! s "variants" "-birds-a.txt.edn")
    (is (= [] (variants s)) "the buffer visiting the file has them, wherever it is")
    (open! s "/birds/a.txt" "")
    (is (= "/birds/a.txt" (:path (t/app s))))
    (is (= [{:start 2 :end 9 :options ["testing" "tested"] :selected 0}] (variants s))
        "but not those overlapping one before, or showing what isn't there")
    (t/send! s {:type :mode-data :mode "variants" :file "-birds-a.txt.edn" :error "Bad EDN"})
    (is (= "Can't read variants: Bad EDN" (:message (t/app s))))))

(deftest variants-is-a-minor-mode
  (with-session [s]
    (variant-file! s "/birds/a.txt" "a testing word")
    (is (= #{"variants"} (:minor-modes (t/app s))) "on to begin with")
    (t/command! s "minor")
    (is (= "Minor modes: search (off), variants (on)" (:message (t/app s))))
    (t/command! s "minor variants")
    (is (= "Variants mode off" (:message (t/app s))))
    (select-word-at! s 2)
    (t/type! s "v")
    (is (= :normal (:mode (t/app s))) "off, v is nothing")
    (t/command! s "minor variants")
    (is (= [["variants" "-birds-a.txt.edn"] ["variants" "-birds-a.txt.edn"]] (:data-asked @s))
        "on again, it asks for the file's")
    (t/command! s "minor nothing")
    (is (= "No such minor mode: nothing" (:message (t/app s))))
    (t/command! s "mode variants")
    (is (= "variants is a minor mode: :minor variants turns it on or off" (:message (t/app s))))
    (is (nil? (:major-mode (t/app s))))))

(deftest d-dims-the-selection
  (with-session [s]
    (variant-file! s "/birds/a.txt" "a testing word")
    (select-word-at! s 2)
    (t/type! s "d")
    (is (= "Dimmed" (:message (t/app s))))
    (is (= "testing" (t/selected s)) "the selection stays")
    (t/press! s sdl/K-ESCAPE)
    (select-word-at! s 10)
    (t/type! s "d")
    (t/press! s sdl/K-ESCAPE)
    (t/command! s "w")
    (is (= [{:start 2 :end 9} {:start 10 :end 14}]
           (get-in @s [:mode-data ["variants" "-birds-a.txt.edn"] :dims]))
        "kept beside the file")
    (select-word-at! s 2)
    (t/type! s "d")
    (is (= "Undimmed" (:message (t/app s))) "all dim already, undimmed")
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-UP cmd)
    (t/type! s "d")
    (is (= "Select the text to dim" (:message (t/app s))))
    (dotimes [_ 11] (t/press! s sdl/K-RIGHT))
    (t/type! s "d")
    (is (= "Undimmed" (:message (t/app s))) "with no selection, what the caret is over")
    (t/command! s "w")
    (is (= {} (:mode-data @s)) "none left to keep")
    (testing "read back"
      (swap! s assoc-in [:mode-data ["variants" "-birds-b.txt.edn"]]
             {:variants [] :dims [{:start 0 :end 1} {:start 3 :end 99}]})
      (variant-file! s "/birds/b.txt" "abcdef")
      (t/data-read! s "variants" "-birds-b.txt.edn")
      (t/command! s "w")
      (is (= [{:start 0 :end 1}] (get-in @s [:mode-data ["variants" "-birds-b.txt.edn"] :dims]))
          "but not what is past the text's end"))))

(deftest variants-keep-out-of-insets
  (with-session [s :mode :normal]
    (auk! s {:content ["a line" {:type :list :content [{:text "an item"}]}]})
    (t/press! s sdl/K-DOWN)
    (is (some? (insets/path (t/app s))))
    (t/type! s "w")
    (t/type! s "v")
    (is (= "Variants are only of the buffer's own text" (:message (t/app s))))))

;; ---------------------------------------------------------------- the mark

(deftest m-sets-the-mark-and-selects-from-it
  (with-session [s]
    (t/type! s "one two three")
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-UP cmd)
    (t/type! s "m")
    (is (= "Mark set" (:message (t/app s))))
    (is (nil? (t/selected s)) "nothing selected yet")
    (dotimes [_ 3] (t/press! s sdl/K-RIGHT))
    (is (= "one" (t/selected s)) "moving selects from it")
    (t/press! s sdl/K-RIGHT cmd)
    (is (= "one " (t/selected s)))
    (t/press! s sdl/K-A sdl/KMOD-CTRL)
    (is (nil? (t/selected s)) "back at the mark, nothing")
    (t/press! s sdl/K-DOWN)
    (is (= "one two three" (t/selected s)))
    (t/press! s sdl/K-ESCAPE)
    (is (nil? (t/selected s)) "escape ends it")
    (t/press! s sdl/K-LEFT)
    (is (nil? (t/selected s)) "and moving no longer selects")
    (testing "c and x leave nothing selected, and the mark not active"
      (t/press! s sdl/K-UP cmd)
      (t/type! s "m")
      (dotimes [_ 3] (t/press! s sdl/K-RIGHT))
      (t/type! s "c")
      (is (= "one" (:clipboard @s)))
      (is (nil? (t/selected s)))
      (t/press! s sdl/K-RIGHT)
      (is (nil? (t/selected s)))
      (t/type! s "m")
      (dotimes [_ 4] (t/press! s sdl/K-RIGHT))
      (t/type! s "x")
      (is (= "one three" (t/text s)))
      (is (nil? (t/selected s)))
      (t/press! s sdl/K-RIGHT)
      (is (nil? (t/selected s))))
    (testing "insert mode ends it"
      (t/type! s "m")
      (t/type! s "i")
      (t/press! s sdl/K-ESCAPE)
      (t/press! s sdl/K-RIGHT)
      (is (nil? (t/selected s))))))

(deftest j-jumps-back-to-the-mark
  (with-session [s]
    (t/type! s "one two three")
    (t/press! s sdl/K-ESCAPE)
    (t/type! s "j")
    (is (= "No mark set" (:message (t/app s))))
    (t/press! s sdl/K-UP cmd)
    (t/type! s "m")
    (t/press! s sdl/K-ESCAPE)
    (dotimes [_ 4] (t/press! s sdl/K-RIGHT))
    (t/type! s "m")
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-DOWN cmd)
    (t/type! s "j")
    (is (= 4 (t/caret s)) "to the mark")
    (is (nil? (t/selected s)) "selecting nothing")
    (t/press! s sdl/K-RIGHT)
    (is (nil? (t/selected s)) "with the mark not active")
    (t/type! s "j")
    (is (= 0 (t/caret s)) "then the one before")
    (t/type! s "j")
    (is (= 4 (t/caret s)) "and round again")
    (testing "marks move with the text"
      (t/press! s sdl/K-UP cmd)
      (t/type! s "i")
      (t/type! s "zz")
      (t/press! s sdl/K-ESCAPE)
      (t/press! s sdl/K-DOWN cmd)
      (t/type! s "j")
      (is (= 0 (t/caret s)) "as Emacs's: what is typed at a mark goes after it")
      (t/type! s "j")
      (is (= 6 (t/caret s)) "and what is typed before one pushes it along"))))

(deftest the-mark-ring-keeps-sixteen
  (let [doc (reduce (fn [d i] (ed/set-mark (ed/move d i))) (ed/doc "abcdefghijklmnopqrstuvwxyz") (range 20))]
    (is (= 16 (count (:mark-ring doc))))
    (is (= 17 (count (:marks doc))) "the oldest let go")
    (is (= [19 18 17] (->> (iterate ed/jump-to-mark doc) rest (take 3) (map :caret))))))

;; ---------------------------------------------------------------- auk rules

(deftest h-adds-a-rule
  (with-session [s :mode :normal]
    (auk! s {:content ["one" "two"]})
    (t/type! s "h")
    (is (= [{:after 0 :text "" :insets [] :kind :rule}] (insets/snapshot (t/app s))) "below the line")
    (is (= "one\ntwo" (t/text s)) "with no new line: there is a line after it")
    (is (nil? (insets/path (t/app s))) "the caret not in it")
    (is (= 4 (t/caret s)) "but on the line after it")
    (t/command! s "w")
    (is (= "{:content\n [\"one\"\n  {:type :hr}\n  \"two\"]}\n" (get-in @s [:files "/notes/n.auk"])))
    (testing "the caret goes over it, as over a folded section"
      (t/press! s sdl/K-UP)
      (is (insets/over (t/app s)))
      (t/type! s " ")
      (is (insets/over (t/app s)) "space doesn't unfold it")
      (t/press! s sdl/K-UP)
      (is (nil? (insets/path (t/app s))))
      (is (= 0 (t/caret s))))
    (testing "and deletes it, once asked"
      (t/press! s sdl/K-DOWN)
      (t/press! s sdl/K-K (bit-or cmd sdl/KMOD-SHIFT))
      (is (= "Delete this rule? (y/n)" (:prompt (:confirm (t/app s)))))
      (t/type! s "y")
      (is (= [] (insets/snapshot (t/app s))))))
  (testing "on the last line, with a new line after it"
    (with-session [s :mode :normal]
      (auk! s {:content ["one"]})
      (t/type! s "h")
      (is (= "one\n" (t/text s)))
      (is (= 4 (t/caret s)))))
  (testing "on an empty line, in its place"
    (with-session [s :mode :normal]
      (auk! s {:content ["one" "" "two"]})
      (t/press! s sdl/K-DOWN)
      (t/type! s "h")
      (is (= "one\n\ntwo" (t/text s)))
      (is (= [{:after 0 :text "" :insets [] :kind :rule}] (insets/snapshot (t/app s))))
      (is (= 4 (t/caret s)))))
  (testing "read back, in a section"
    (with-session [s :mode :normal]
      (auk! s {:content ["one" (ref 1)] :sections [{:id 1 :content ["a" {:type :hr} "b"]}]})
      (is (= [{:after 0 :text "a\nb"
               :insets [{:after 0 :text "" :insets [] :kind :rule}]}]
             (insets/snapshot (t/app s))))))
  (testing "in a list, after it"
    (with-session [s :mode :normal]
      (auk! s {:content ["one" {:type :list :content [{:text "a"}]} "two"]})
      (t/press! s sdl/K-DOWN)
      (t/type! s "h")
      (is (= [:list :rule] (map :kind (insets/snapshot (t/app s)))))
      (is (nil? (insets/path (t/app s))))
      (is (= 4 (t/caret s))))))

(deftest searching-the-buffer
  (with-session [s :mode :normal]
    (open! s "/birds/a.txt" "one cat\ntwo Cat\nthree cat\n")
    (t/type! s "/")
    (is (= :command (:mode (t/app s))))
    (t/type! s "cat")
    (t/press! s sdl/K-ESCAPE)
    (is (= :normal (:mode (t/app s))))
    (is (not (contains? (:minor-modes (t/app s)) "search")) "escape cancels")
    (t/type! s "/")
    (t/type! s "cat")
    (t/press! s sdl/K-RETURN)
    (is (contains? (:minor-modes (t/app s)) "search"))
    (is (= 4 (get-in (t/app s) [:doc :caret])) "first match from the caret")
    (t/type! s "n")
    (is (= 12 (get-in (t/app s) [:doc :caret])) "case-insensitive")
    (t/type! s "n")
    (t/type! s "n")
    (is (= 4 (get-in (t/app s) [:doc :caret])) "n wraps")
    (t/type! s "N")
    (is (= 22 (get-in (t/app s) [:doc :caret])) "p wraps")
    (t/type! s "/")
    (is (= "cat" (:command (t/app s))) "the query is kept to change")
    (t/type! s "s")
    (t/press! s sdl/K-ESCAPE)
    (is (contains? (:minor-modes (t/app s)) "search") "escape at the prompt keeps the mode")
    (t/press! s sdl/K-ESCAPE)
    (is (not (contains? (:minor-modes (t/app s)) "search")))
    (t/type! s "/")
    (t/type! s "zzz")
    (t/press! s sdl/K-RETURN)
    (is (= "No results: zzz" (:message (t/app s))))))
