(ns hoatzin.render-test
  "Golden-image tests: render headlessly and diff against test/golden.
  See hoatzin.test-support for regenerating goldens."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hoatzin.app :as app]
            [hoatzin.app.dropdown :as dropdown]
            [hoatzin.lib.sdl :as sdl]
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
    (t/advance! s 2000)
    (t/send! s {:type :tick})
    (t/send! s {:type :move :x 790.0 :y 300.0})
    (t/matches-golden? "scrolled-selection-and-hovered-scrollbar" (t/render! s))))

(deftest ^:integration normal-mode
  ;; a fresh editor: an empty block caret, NORMAL in the status bar
  (with-session [s :mode nil]
    (t/matches-golden? "normal-mode-empty" (t/render! s))))

(deftest ^:integration block-caret
  ;; the block covers the character after the caret, which shows through
  ;; inverted, and sits past the end of a line where there is none
  (with-session [s]
    (t/type! s hoatzin-text)
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-UP cmd)
    (dotimes [_ 4] (t/press! s sdl/K-RIGHT))
    (t/matches-golden? "block-caret" (t/render! s))
    (t/press! s sdl/K-END)
    (t/matches-golden? "block-caret-at-a-line-end" (t/render! s))))

(deftest ^:integration block-caret-in-a-light-theme
  ;; black text on white: the block is black, and the character in it white
  (with-session [s]
    (swap! s assoc-in [:app :settings :theme] "Light 1")
    (t/type! s hoatzin-text)
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-UP cmd)
    (dotimes [_ 4] (t/press! s sdl/K-RIGHT))
    (t/matches-golden? "block-caret-light" (t/render! s))))

(deftest ^:integration command-line
  (with-session [s]
    (t/type! s hoatzin-text)
    (t/press! s sdl/K-ESCAPE)
    (t/type! s ":open")
    (t/matches-golden? "command-line" (t/render! s))))

(deftest ^:integration opened-file
  (with-session [s :mode nil]
    (t/send! s {:type :opened :path "/birds/hoatzin.txt"
                :text (str hoatzin-text "\n\nIt eats leaves.\n")})
    (t/matches-golden? "opened-file" (t/render! s))))

(deftest ^:integration boxes
  ;; a block below the first paragraph, pushing the next down, and a float
  ;; in the top right corner over the text
  (with-session [s :height 360]
    (t/type! s (str hoatzin-text "\n\nIt eats leaves."))
    (swap! s update :app
           #(-> %
                (app/add-block :form 0
                               {:kind :box
                                :style {:border 1 :padding 6 :gap 6 :background [40 40 60]}
                                :children [{:kind :label :text "Field notes"}
                                           {:kind :box :style {:direction :row :gap 6 :align :center}
                                            :children [{:kind :field :id :name :value "Opisthocomus"
                                                        :style {:grow 1}}
                                                       {:kind :checkbox :id :seen :value true}
                                                       {:kind :button :id :save :text "Save"}]}]})
                (app/set-floats [{:kind :box
                                  :style {:position :absolute :top 8 :right 8 :padding [4 8]
                                          :border 1 :background [60 50 70]
                                          :border-color [200 140 120]}
                                  :children [{:kind :label :text "Float"}]}])
                app/settle))
    (t/matches-golden? "boxes" (t/render! s))))

(deftest ^:integration command-hints
  ;; the commands `:` could begin, in a box above the status bar; typing
  ;; narrows them down
  (with-session [s]
    (t/type! s hoatzin-text)
    (t/press! s sdl/K-ESCAPE)
    (t/type! s ":")
    (t/matches-golden? "command-hints" (t/render! s))
    (t/type! s "w")
    (t/matches-golden? "command-hints-filtered" (t/render! s))))

(deftest ^:integration buffers
  ;; the buffers window over the text: the current buffer marked, the one
  ;; it is on highlighted, the unsaved one with [+], and each one's file
  ;; or directory dimmed
  (with-session [s :mode nil :dir "/home/hoatzin"]
    (t/send! s {:type :opened :path "/birds/hoatzin.txt" :text hoatzin-text})
    (t/type! s "i")
    (t/type! s "Note: ")
    (t/press! s sdl/K-ESCAPE)
    (t/send! s {:type :opened :path "/fish/notes.txt" :text "Bream."})
    (t/command! s "new")
    (t/command! s "buffers")
    (t/type! s "t")
    (t/type! s "draft")
    (t/press! s sdl/K-RETURN)
    (t/press! s sdl/K-UP)
    (t/type! s "t")
    (t/type! s "fis")
    (t/matches-golden? "buffers" (t/render! s))
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-UP)
    (t/type! s "p")
    (t/matches-golden? "buffers-preview" (t/render! s))))

(deftest ^:integration settings
  ;; the settings window over the text, inset by the margin
  (with-session [s]
    (t/type! s hoatzin-text)
    (t/press! s sdl/K-ESCAPE)
    (t/command! s "settings")
    (t/matches-golden? "settings" (t/render! s))))

(defn- click-field!
  "Click the middle of the settings window's field `id`."
  [s id]
  (let [{[x y w h] :rect} (some #(when (= id (get-in % [:node :id])) %)
                                (:float-places (t/app s)))]
    (t/click! s (double (+ x (quot w 2))) (double (+ y (quot h 2))))))

(deftest ^:integration settings-font-list
  ;; the editor font's list open, scrolled a little, the pointer on an
  ;; option, each in its own font; Georgia, the one chosen, ticked
  (with-session [s]
    (t/type! s hoatzin-text)
    (t/press! s sdl/K-ESCAPE)
    (t/command! s "settings")
    (click-field! s :settings/editor-family)
    (t/send! s {:type :wheel :dy -0.4})
    (t/advance! s 1000)
    (let [{[ix iy iw] :inner :keys [row-h]} (dropdown/place (t/app s))
          scroll (:scroll (:list (t/app s)))]
      (t/send! s {:type :tick}
               {:type :move :x (double (+ ix (quot iw 2)))
                :y (double (- (+ iy (* 8 row-h) (quot row-h 2)) scroll))}))
    (t/matches-golden? "settings-font-list" (t/render! s))))

(deftest ^:integration settings-theme-list
  ;; the theme list open, the pointer on Light 3, the theme in use ticked
  (with-session [s]
    (t/type! s hoatzin-text)
    (t/press! s sdl/K-ESCAPE)
    (t/command! s "settings")
    (click-field! s :settings/theme)
    (let [{[ix iy iw] :inner :keys [row-h]} (dropdown/place (t/app s))]
      (t/send! s {:type :move :x (double (+ ix (quot iw 2)))
                  :y (double (+ iy (* 2 row-h) (quot row-h 2)))}))
    (t/matches-golden? "settings-theme-list" (t/render! s))))

(deftest ^:integration settings-editing
  ;; a font size field with the focus, being typed into; then the window
  ;; closed on the sizes it set
  (with-session [s]
    (t/type! s hoatzin-text)
    (t/press! s sdl/K-ESCAPE)
    (t/command! s "settings")
    (click-field! s :settings/editor-size)
    (t/press! s sdl/K-BACKSPACE)
    (t/type! s "6")
    (t/press! s sdl/K-TAB)                 ; past the UI font's dropdown
    (t/press! s sdl/K-TAB)
    (t/press! s sdl/K-UP)
    (t/press! s sdl/K-UP)
    (t/press! s sdl/K-UP)
    (t/matches-golden? "settings-editing" (t/render! s))
    (t/press! s sdl/K-ESCAPE)
    (t/advance! s 150)                  ; the editor font's wait
    (t/send! s {:type :tick})
    (t/matches-golden? "settings-applied" (t/render! s))))

;; ---------------------------------------------------------------- auk mode

(def ^:private section-text
  (str "Hoatzin chicks have claws on two of their wing digits, which they use "
       "to climb back up to the nest after dropping into the water."))

(deftest ^:integration auk-section
  ;; a section added below the first line, typed into until it wraps: the
  ;; caret, a block in normal mode, in it, and the line below pushed down
  (with-session [s :mode :normal :height 360]
    (t/send! s {:type :opened :path "/notes/hoatzin.auk"
                :text (pr-str {:content [hoatzin-text "It eats leaves."]})})
    (t/press! s sdl/K-S cmd)
    (t/type! s "i")
    (t/type! s section-text)
    (t/press! s sdl/K-ESCAPE)
    (t/matches-golden? "auk-section" (t/render! s))))

(deftest ^:integration auk-sections-folded-and-scrolled
  ;; a folded section, with its first line and its length in its header,
  ;; then one of more lines than it shows, scrolled, with its thumb; the
  ;; caret in the buffer's text
  (with-session [s :mode :normal :height 420]
    (t/send! s {:type :opened :path "/notes/hoatzin.auk"
                :text (pr-str {:content [{:type :section :ref 1} "Between them." {:type :section :ref 2}
                                         "After them."]
                               :sections [{:id 1 :content ["Folded away" "and more"]}
                                          {:id 2 :content (mapv #(str "Line " %) (range 1 16))}]})})
    (let [header (fn [id] (:header (some #(when (= id (:inset %)) %) (:block-places (t/app s)))))
          m 48
          click-header! (fn [id] (let [[x y _ h] (header id)]
                                   (t/click! s (double (+ m x 20)) (double (+ m y (quot h 2))))))]
      (click-header! 0)
      (let [[x y] (header 1)]
        (t/send! s {:type :move :x (double (+ m x 100)) :y (double (+ m y 100))}
                 {:type :wheel :dy -1.0})))
    (t/matches-golden? "auk-sections-folded-and-scrolled" (t/render! s))))

(deftest ^:integration auk-delete-asks
  ;; cmd+shift+k in a section asks in the status bar
  (with-session [s :mode :normal]
    (t/send! s {:type :opened :path "/notes/hoatzin.auk"
                :text (pr-str {:content [hoatzin-text {:type :section :ref 1}]
                               :sections [{:id 1 :content ["Gone soon."]}]})})
    (t/press! s sdl/K-DOWN cmd)
    (t/press! s sdl/K-DOWN)
    (t/press! s sdl/K-K (bit-or cmd sdl/KMOD-SHIFT))
    (t/matches-golden? "auk-delete-asks" (t/render! s))))

(deftest ^:integration auk-nested-sections
  ;; a section in a section, the caret in it, and the new line after each
  (with-session [s :mode :normal :height 420]
    (t/send! s {:type :opened :path "/notes/hoatzin.auk"
                :text (pr-str {:content ["Hoatzins."]})})
    (t/press! s sdl/K-S cmd)
    (t/type! s "i")
    (t/type! s "Chicks have claws on their wings.")
    (t/press! s sdl/K-ESCAPE)
    (t/press! s sdl/K-S cmd)
    (t/type! s "i")
    (t/type! s "They lose them as adults.")
    (t/press! s sdl/K-ESCAPE)
    (t/matches-golden? "auk-nested-sections" (t/render! s))))

(deftest ^:integration auk-help
  ;; in auk mode, its keys come first in the help
  (with-session [s :mode :normal :height 400]
    (t/send! s {:type :opened :path "/notes/hoatzin.auk" :text "{:content []}"})
    (t/type! s "?")
    (t/matches-golden? "auk-help" (t/render! s))))

(deftest ^:integration auk-over-and-renaming
  ;; the caret over a folded section, its header highlighted; then the
  ;; other being renamed, the caret in its header
  (with-session [s :mode :normal :height 360]
    (t/send! s {:type :opened :path "/notes/hoatzin.auk"
                :text (pr-str {:content ["Hoatzins." {:type :section :ref 1} {:type :section :ref 2}]
                               :sections [{:id 1 :title "Habitat" :content ["Swamps and mangroves."]}
                                          {:id 2 :content ["Leaves, mostly."]}]})})
    (t/press! s sdl/K-DOWN)
    (t/type! s " ")
    (t/matches-golden? "auk-over-folded" (t/render! s))
    (t/press! s sdl/K-DOWN)
    (t/press! s (int \r) cmd)
    (t/type! s "Diet")
    (t/matches-golden? "auk-renaming" (t/render! s))))

(deftest ^:integration auk-lists
  ;; a list and a checklist, one item ticked, an item wrapping
  (with-session [s :mode :normal :height 400]
    (t/send! s {:type :opened :path "/notes/hoatzin.auk"
                :text (pr-str {:content ["Eats:"
                                         {:type :list :content [{:text "leaves"}
                                                                {:text (str "flowers and fruit, fermented in "
                                                                            "its crop, which makes it smell")}]}
                                         "To see:"
                                         {:type :checklist :content [{:text "a chick" :checked? true}
                                                                     {:text "its claws" :checked? false}]}]})})
    (t/matches-golden? "auk-lists" (t/render! s))))

(deftest ^:integration auk-nested-lists
  ;; a list with a sublist, holding a checklist, each indented further
  (with-session [s :mode :normal :height 360]
    (t/send! s {:type :opened :path "/notes/hoatzin.auk"
                :text (pr-str {:content ["Eats:"
                                         {:type :list
                                          :content [{:text "leaves"}
                                                    {:type :list
                                                     :content [{:text "young ones"}
                                                               {:type :checklist
                                                                :content [{:text "seen" :checked? true}
                                                                          {:text "photographed"}]}]}
                                                    {:text "flowers"}]}]})})
    (t/matches-golden? "auk-nested-lists" (t/render! s))))

(deftest ^:integration variants
  ;; a variant of three wordings, and one of two, each with a dot for each
  ;; below its start; then a variant being typed, the caret an underline
  (with-session [s :height 160]
    (t/send! s {:type :opened :path "/birds/hoatzin.txt" :text "The hoatzin is a stinky bird."})
    (t/send! s {:type :mode-data :mode "variants" :file "-birds-hoatzin.txt.edn"
                :data {:variants [{:start 4 :end 11 :options ["hoatzin" "stinkbird" "canje pheasant"]
                                   :selected 0}
                                  {:start 17 :end 23 :options ["smelly" "stinky"] :selected 1}]
                       :dims [{:start 24 :end 28}]}})
    (t/press! s sdl/K-ESCAPE)
    (t/matches-golden? "variants" (t/render! s))
    (t/press! s sdl/K-RIGHT cmd)
    (t/type! s "w")
    (t/type! s "vbro")
    (t/matches-golden? "variant-typed" (t/render! s))))

(deftest ^:integration a-variant-across-lines
  ;; a long variant, tinted across each line it wraps onto
  (with-session [s :height 200]
    (t/send! s {:type :opened :path "/birds/hoatzin.txt" :text hoatzin-text})
    (t/send! s {:type :mode-data :mode "variants" :file "-birds-hoatzin.txt.edn"
                :data {:variants [{:start 44 :end 112
                                   :options [(subs hoatzin-text 44 112) "found in South America"]
                                   :selected 0}]}})
    (t/press! s sdl/K-ESCAPE)
    (t/matches-golden? "variant-across-lines" (t/render! s))))

(deftest ^:integration auk-rules
  ;; two rules, the caret over the second, which is highlighted
  (with-session [s :mode :normal :height 300]
    (t/send! s {:type :opened :path "/notes/hoatzin.auk"
                :text (pr-str {:content ["Hoatzins." {:type :hr} "Swamps." {:type :hr} "Leaves."]})})
    (t/press! s sdl/K-DOWN)
    (t/press! s sdl/K-DOWN)
    (t/press! s sdl/K-DOWN)
    (t/matches-golden? "auk-rules" (t/render! s))))

(deftest ^:integration auk-selection-across-insets
  ;; one selection from the first line, through a list and a section, to the
  ;; line after them: every text shows its part
  (with-session [s :mode :normal :height 420]
    (t/send! s {:type :opened :path "/notes/hoatzin.auk"
                :text (pr-str {:content ["Hoatzins eat:"
                                         {:type :list :content [{:text "leaves"} {:text "flowers"}]}
                                         {:type :section :ref 1}
                                         "and are smelly."]
                               :sections [{:id 1 :title "Habitat" :content ["Swamps and mangroves."]}]})})
    (t/press! s sdl/K-RIGHT sdl/KMOD-SHIFT)
    (dotimes [_ 6] (t/press! s sdl/K-DOWN sdl/KMOD-SHIFT))
    (t/matches-golden? "auk-selection-across-insets" (t/render! s))))

(deftest ^:integration auk-before-a-section
  ;; the caret before a section with nothing above it: its header highlighted
  (with-session [s :mode :normal :height 300]
    (t/send! s {:type :opened :path "/notes/hoatzin.auk"
                :text (pr-str {:content [{:type :section :ref 1} "Hoatzins."]
                               :sections [{:id 1 :title "Habitat" :content ["Swamps and mangroves."]}]})})
    (t/press! s sdl/K-UP)
    (t/press! s sdl/K-UP)
    (t/matches-golden? "auk-before-a-section" (t/render! s))))

;; ---------------------------------------------------------------- auk export

(def ^:private export-sample
  (str "{:content\n [{:type :section :ref 1}\n  \"then we have:\"\n"
       "  {:type :list :content [{:text \"a list\"} {:type :list :content [{:text \"with a sublist\"}]}]}\n"
       "  \"and more text \"\n  {:type :section :ref 2}\n  \"\"\n  \"\"]\n :sections\n"
       " [{:id 1 :title \"header\" :content [\"first section\"]}\n"
       "  {:id 2 :title \"footer\" :content [\"then a section to finish\"]}]}\n"))

(deftest ^:integration auk-export-hints
  (with-session [s :mode :normal :height 300]
    (t/send! s {:type :opened :path "/notes/hoatzin.auk" :text export-sample})
    (t/type! s "e")
    (t/matches-golden? "auk-export-hints" (t/render! s))))

(deftest auk-export
  (with-session [s :mode :normal]
    (t/send! s {:type :opened :path "/notes/hoatzin.auk" :text export-sample})
    (t/type! s "e")
    (t/type! s "1")
    (is (= (str "> **header**\n>\n> first section\n\nthen we have:\n\n"
                "* a list\n  * with a sublist\n\nand more text\n\n"
                "> **footer**\n>\n> then a section to finish\n")
           (get-in @s [:files "/notes/hoatzin.md"])))
    (t/type! s "e")
    (t/type! s "2")
    (is (= (str "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n"
                "  <meta charset=\"utf-8\">\n"
                "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
                "  <title>hoatzin</title>\n</head>\n<body>\n"
                "  <section>\n    <h2>header</h2>\n    <p>first section</p>\n  </section>\n"
                "  <p>then we have:</p>\n  <ul>\n    <li>a list\n      <ul>\n"
                "        <li>with a sublist</li>\n      </ul>\n    </li>\n  </ul>\n"
                "  <p>and more text</p>\n"
                "  <section>\n    <h2>footer</h2>\n    <p>then a section to finish</p>\n  </section>\n"
                "</body>\n</html>\n")
           (get-in @s [:files "/notes/hoatzin.html"])))))

(deftest auk-export-checklists-and-rules
  (with-session [s :mode :normal]
    (t/send! s {:type :opened :path "/notes/a.auk"
                :text (pr-str {:content [{:type :checklist :content [{:text "done" :checked? true} {:text "todo"}]}
                                         {:type :hr} "end"]})})
    (t/type! s "e")
    (t/type! s "1")
    (is (= "- [x] done\n- [ ] todo\n\n---\n\nend\n" (get-in @s [:files "/notes/a.md"])))
    (t/type! s "e")
    (t/type! s "2")
    (is (str/includes? (get-in @s [:files "/notes/a.html"])
                       (str "<li><input type=\"checkbox\" checked onclick=\"return false;\"> done</li>\n"
                            "    <li><input type=\"checkbox\" onclick=\"return false;\"> todo</li>")))
    (is (str/includes? (get-in @s [:files "/notes/a.html"]) "<hr>"))))

(deftest auk-export-cancelled
  (with-session [s :mode :normal]
    (t/send! s {:type :opened :path "/notes/a.auk" :text "{:content [\"x\"]}"})
    (t/type! s "e")
    (t/type! s "3")
    (is (nil? (:choose (t/app s))))
    (is (empty? (:files @s)))))
