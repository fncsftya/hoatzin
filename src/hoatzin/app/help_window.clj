(ns hoatzin.app.help-window
  "The help window: the commands and the keys that run them, over the text,
  opened by `?` in normal mode and closed by escape. The current buffer's
  mode's own come first (see hoatzin.app.modes)."
  (:require [hoatzin.app.face :refer [ui-width]]
            [hoatzin.app.geometry :refer [status-height]]
            [hoatzin.app.modes :as modes]
            [hoatzin.lib.sdl :as sdl]))

(def commands
  "The commands as [keys description] in sections of [title rows], as they
  are bound in hoatzin.app.input.keyboard and hoatzin.app.command."
  [["Normal mode"
    [["i" "insert before the caret"]
     ["a" "insert after the character"]
     ["A" "insert at the end of the line"]
     ["o" "new line below, and insert"]
     ["O" "new line above, and insert"]
     ["w" "select the word; again, extend to the next"]
     ["s" "select the sentence; again, extend to the next"]
     ["c" "copy the selection"]
     ["x" "cut the selection"]
     ["p" "paste at the caret"]
     ["u" "undo"]
     ["r" "redo (after undoing)"]
     ["0" "start of the line"]
     ["^" "first non-blank of the line"]
     ["g" "go to a line number"]
     [":" "open the command line"]
     ["?" "show this help"]]]
   ["Moving (shift extends the selection)"
    [["arrows" "move the caret"]
     ["cmd+left / right" "back / forward a word"]
     ["cmd+up / down" "start / end of the text"]
     ["home / end" "start / end of the line"]
     ["page up / down" "move a page"]
     ["cmd+a" "select all"]
     ["cmd+c" "copy the selection (any mode)"]
     ["esc" "back to normal mode"]]]
   ["Insert mode"
    [["cmd+x" "cut the selection"]
     ["cmd+v" "paste at the caret"]
     ["ctrl+a / ctrl+e" "start / end of the line"]]]
   ["Command line"
    [[":open" "choose a file and visit it in a buffer"]
     [":write" "save the buffer"]
     [":save" "choose where to save the buffer"]
     [":buffers" "list the buffers, to switch to one"]
     [":new" "start an empty buffer"]
     [":close" "close the buffer (:close! to discard changes)"]
     [":revert" "read the buffer's file again"]
     [":cd" "choose the buffer's directory"]
     [":mode name" "put the buffer in a mode (text for none)"]
     [":quit" "quit (:quit! to discard changes)"]
     [":settings" "show the settings"]]]])

(def ^:private padding "Points inside the help window's border." 16)
(def ^:private gap "Points between the help window's rows." 4)

(defn- sections
  "The sections shown: the mode's, then the editor's."
  [app]
  (concat (modes/help app) commands))

(defn- items
  "The sections flattened to the rows that scroll: [:title s] or [:row keys description]."
  [app]
  (vec (mapcat (fn [[title rows]] (cons [:title title] (map #(into [:row] %) rows))) (sections app))))

(defn- geometry
  "The window's height and its rows' height, in points."
  [app]
  (let [d (:density app)]
    [(- (/ (second (:size app)) d) (/ (status-height app) d) (* 2 (:margin app)))
     (/ (get-in app [:ui :metrics :line-height]) d)]))

(defn visible-rows
  "How many rows fit in the window under its heading."
  [app]
  (let [[h lh] (geometry app)]
    (max 1 (long (Math/floor (/ (- h (* 2 padding) 2 lh) (+ lh gap)))))))

(defn max-scroll
  "The most rows the list can be scrolled by."
  [app]
  (max 0 (- (count (items app)) (visible-rows app))))

(defn scroll-by
  "The app with the help scrolled `n` rows, within the list."
  [app n]
  (assoc app :help-scroll (-> (+ (:help-scroll app 0) n) (min (max-scroll app)) (max 0))
         :dirty? true))

(defn on-key
  "A key in the help window: up, down and the page keys scroll it."
  [app key]
  (condp = key
    sdl/K-UP       (scroll-by app -1)
    sdl/K-DOWN     (scroll-by app 1)
    sdl/K-PAGEUP   (scroll-by app (- (visible-rows app)))
    sdl/K-PAGEDOWN (scroll-by app (visible-rows app))
    sdl/K-HOME     (scroll-by app (- (count (items app))))
    sdl/K-END      (scroll-by app (count (items app)))
    app))

(defn on-wheel [app dy]
  (scroll-by app (- (long (Math/signum (double dy))))))

(defn window
  "The help as a box over the text, inset by the margin: the rows from the
  scroll on that fit, each section's title, or its keys in a column as wide
  as the widest and what they do."
  [app]
  (let [m     (:margin app)
        fg    (:foreground app)
        dim   (mapv #(quot (+ (* 2 %1) %2) 3) fg (:window-background app))
        key-w (/ (reduce max (map #(ui-width app (first %)) (mapcat second (sections app))))
                 (:density app))
        start (min (:help-scroll app 0) (max-scroll app))
        shown (take (visible-rows app) (drop start (items app)))
        more? (pos? (max-scroll app))]
    {:kind :box
     :style {:position :absolute :left m :top m :right m
             :bottom (+ (/ (status-height app) (:density app)) m)
             :padding padding :gap gap :border 1
             :background (:window-background app) :border-color (:ui-border app)}
     :children
     (into [{:kind :label :text "Help" :style {:color fg}}
            {:kind :label :text (if more? "up/down to scroll   esc" "esc")
             :style {:position :absolute :top padding :right padding :color dim}}]
           (map (fn [[kind a b]]
                  (if (= kind :title)
                    {:kind :label :text a :style {:color dim}}
                    {:kind :box :style {:direction :row :gap 8}
                     :children [{:kind :label :text a :style {:width key-w :color fg}}
                                {:kind :label :text b :style {:color fg}}]}))
                shown))}))
