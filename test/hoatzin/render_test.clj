(ns hoatzin.render-test
  "Golden-image tests: render headlessly and diff against test/golden.
  See hoatzin.test-support for regenerating goldens."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest]]
            [hoatzin.sdl :as sdl]
            [hoatzin.test-support :as t :refer [with-session]]))

(def cmd sdl/KMOD-GUI)

(def hoatzin-text
  (str "The hoatzin is a species of tropical bird found in the swamps, "
       "riparian forests and mangroves of the Amazon and the Orinoco basins."))

(deftest ^:integration empty-document
  (with-session [s]
    (t/matches-golden? "empty-caret-on" (t/render! s))
    (t/advance! s 530)
    (t/matches-golden? "empty-caret-off" (t/render! s))))

(deftest ^:integration wrapped-paragraph
  (with-session [s]
    (t/type! s hoatzin-text)
    (t/matches-golden? "wrapped-paragraph" (t/render! s))))

(deftest ^:integration caret-after-navigation
  (with-session [s]
    (t/type! s "First paragraph.\n\nThird, after a blank line.")
    (t/press! s sdl/K-UP)
    (t/press! s sdl/K-UP)
    (t/press! s sdl/K-RIGHT cmd)
    (t/matches-golden? "caret-after-navigation" (t/render! s))))

(deftest ^:integration unicode
  ;; Colour emoji must keep their colours; accents compose.
  (with-session [s]
    (t/type! s "Hoatzin 🐦 café naïve é — “quoted” ½")
    (t/matches-golden? "unicode" (t/render! s))))

(deftest ^:integration scrolled-to-the-end
  (with-session [s]
    (t/set-clipboard! s (str/join "\n" (map #(str "Line " % ": the stinkbird.") (range 1 41))))
    (t/press! s sdl/K-V cmd)
    (t/matches-golden? "scrolled-to-the-end" (t/render! s))))

(deftest ^:integration rewrapped-after-resize
  (with-session [s]
    (t/type! s hoatzin-text)
    (t/render! s)
    (t/resize! s 240 300)
    (t/matches-golden? "rewrapped-after-resize" (t/render! s))))

(deftest ^:integration unfocused
  (with-session [s]
    (t/type! s "No caret when the window is in the background.")
    (t/send! s {:type :focus :focused? false})
    (t/matches-golden? "unfocused" (t/render! s))))

(deftest ^:integration density-1x
  (with-session [s :density 1.0]
    (t/type! s hoatzin-text)
    (t/matches-golden? "wrapped-paragraph-1x" (t/render! s))))

(deftest ^:integration dead-key-composition
  ;; option-e after "caf": the accent shows underlined, the caret after it
  (with-session [s]
    (t/type! s "Hoatzins live in the caf")
    (t/compose! s "´" 1)
    (t/matches-golden? "composition-dead-key" (t/render! s))
    (t/send! s {:type :composition :text "" :cursor 0} {:type :text :text "é"})
    (t/matches-golden? "composition-committed" (t/render! s))))

(deftest ^:integration composition-across-a-wrap
  ;; a long composition (as an input method for CJK makes) wraps and is
  ;; underlined on both lines
  (with-session [s]
    (t/type! s "The hoatzin is a bird: ")
    (t/compose! s "ホアツィンは南米の鳥です" 3)
    (t/matches-golden? "composition-across-a-wrap" (t/render! s))))

(deftest ^:integration selection
  ;; from mid-paragraph, across a wrap and a blank line, into the last
  ;; paragraph: highlighted behind the text, with no caret
  (with-session [s]
    (t/type! s (str hoatzin-text "\n\nIt eats leaves."))
    (t/press! s sdl/K-UP cmd)
    (dotimes [_ 30] (t/press! s sdl/K-RIGHT))
    (t/press! s sdl/K-DOWN (bit-or cmd sdl/KMOD-SHIFT))
    (dotimes [_ 7] (t/press! s sdl/K-LEFT sdl/KMOD-SHIFT))
    (t/matches-golden? "selection" (t/render! s))
    (t/send! s {:type :focus :focused? false})
    (t/matches-golden? "selection-unfocused" (t/render! s))))

(deftest ^:integration caret-at-a-wrapped-line-end
  ;; clicking past the end of a wrapped line puts the caret after its space,
  ;; still on that line
  (with-session [s :width 800 :height 300]
    (t/type! s (apply str (repeat 8 "dfffasaghdignisaopgndispagndipagnipasgnid ")))
    (t/click! s 5000.0 (+ (* 2 24) 10.0))
    (t/matches-golden? "caret-at-a-wrapped-line-end" (t/render! s))))

(deftest ^:integration double-clicked-word
  (with-session [s]
    (t/type! s hoatzin-text)
    (t/double-click! s 200.0 (+ (* 2 24) 10.0))
    (t/matches-golden? "double-clicked-word" (t/render! s))))

(deftest ^:integration scrolled-selection-and-hovered-scrollbar
  ;; a selection running off both ends of the view, scrolled to the middle,
  ;; with the pointer over the scroll bar: it widens over its track
  (with-session [s]
    (t/set-clipboard! s (str/join "\n" (map #(str "Line " % ": the stinkbird.") (range 1 41))))
    (t/press! s sdl/K-V cmd)
    (t/press! s sdl/K-A cmd)
    (t/send! s {:type :wheel :dy 5})
    (t/send! s {:type :move :x 790.0 :y 300.0})
    (t/matches-golden? "scrolled-selection-and-hovered-scrollbar" (t/render! s))))
