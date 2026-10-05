(ns hoatzin.app.command
  "The command line: `:` in normal mode opens it in the status bar,
  return runs it and escape abandons it. While it is open, a box above
  the status bar hints at the commands what is typed could still be.

  A command runs from any prefix that begins no other, so `:w` is
  `:write`, and tab completes as far as the commands it could be agree."
  (:require [clojure.string :as str]
            [hoatzin.app.buffers :as buffers]
            [hoatzin.app.buffers-window :as buffers-window]
            [hoatzin.app.face :refer [ui-width]]
            [hoatzin.app.files :as files]
            [hoatzin.app.geometry :refer [status-height]]
            [hoatzin.app.settings-window :as settings-window]
            [hoatzin.app.state :refer [px command? enter-mode touched]]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.text :as text]
            [hoatzin.lib.sdl :as sdl]))

(defn line-text
  "The command line as shown: its text after the `:`, or after the prompt
  for a line number."
  [app]
  (str (if (:goto? app) "Go to line: " ":") (:command app)))

;; ---------------------------------------------------------------- commands

(defn- quit
  "Quit, unless a buffer has unsaved changes and not `force?`."
  [app _ force?]
  (let [bs      (buffers/listing app)
        names   (buffers/names bs)
        unsaved (filter buffers/unsaved? bs)]
    (if (and (seq unsaved) (not force?))
      (assoc app :message (str "Unsaved changes in "
                               (str/join ", " (map #(str "\"" (names (:buffer-id %)) "\"") unsaved))
                               " (add ! to override)"))
      (assoc app :quit? true))))

;; Each command is (fn [app now force?]), `force?` being a trailing `!`.
(def ^:private commands
  {"open"  (fn [app _ _] (files/open app))
   "write" (fn [app _ _] (if-let [path (:path app)] (files/write-file app path) (files/save-as app)))
   "save"  (fn [app _ _] (files/save-as app))
   "quit"  quit
   "buffers" (fn [app _ _] (buffers-window/open app))
   "new"   (fn [app now _] (buffers/new-buffer app now))
   "close" buffers/close
   "revert" (fn [app now _] (buffers/revert app now))
   "cd"    (fn [app _ _] (buffers/cd app))
   "settings" (fn [app _ _] (settings-window/open app))})

(defn- names-beginning
  "The commands' names that begin with `typed`, alphabetically."
  [typed]
  (filterv #(str/starts-with? % typed) (sort (keys commands))))

(defn- command-names
  "The commands `typed` could mean: the one it names, else those it begins."
  [typed]
  (if (contains? commands typed)
    [typed]
    (names-beginning typed)))

(defn- parse-command
  "The command line as [command force?], `force?` being a trailing `!`."
  [line]
  (let [typed  (str/trim line)
        force? (str/ends-with? typed "!")]
    [(str/trim (cond-> typed force? (subs 0 (dec (count typed))))) force?]))

;; ---------------------------------------------------------------- editing it

(defn open-line
  "Open the command line, empty."
  [app now]
  (-> app (enter-mode now :command) (assoc :command "")))

(defn open-goto
  "Open the command line to ask for a line number."
  [app now]
  (assoc (open-line app now) :goto? true))

(defn- leave-line [app now]
  (-> app (enter-mode now :normal) (dissoc :command :goto?)))

(defn- goto-line
  "Move the caret to the start of the 1-based line typed, clamped to the
  text's lines; anything but a number leaves it where it is."
  [app now]
  (if-let [n (parse-long (str/trim (:command app)))]
    (let [t (text/of (get-in app [:doc :text]))
          k (-> n dec (max 0) (min (dec (text/line-count t))))]
      (-> app
          (assoc :doc (ed/move (:doc app) (text/line-start t k)) :goal-x nil :upstream? false)
          (touched now)))
    app))

(declare run-command)

(defn- run-line
  "Run the command line, back in normal mode."
  [app now]
  (if (:goto? app)
    (leave-line (goto-line app now) now)
    (run-command app now)))

(defn- run-command [app now]
  (let [[command force?] (parse-command (:command app))
        app     (leave-line app now)
        names   (command-names command)]
    (cond
      (= "" command)     app
      (= 1 (count names)) ((commands (first names)) app now force?)
      (seq names)        (assoc app :message (str "Ambiguous command: " command
                                                  " (" (str/join ", " names) ")"))
      :else              (assoc app :message (str "Not an editor command: " command)))))

(defn- shared-start [a b]
  (subs a 0 (count (take-while true? (map = a b)))))

(defn- complete
  "The command line completed as far as the commands it could mean agree."
  [command]
  (let [names (command-names (str/triml command))]
    (if (seq names) (reduce shared-start names) command)))

(defn on-text
  "Typed text, onto the end of the command line."
  [app now text]
  (let [text (if (:goto? app) (apply str (filter #(Character/isDigit ^char %) text)) text)]
    (-> app (update :command str text) (assoc :dirty? true :blink-from now))))

(defn on-key
  "A key on the command line. Other keys leave the document alone."
  [app now key]
  (let [command (:command app)]
    (condp = key
      sdl/K-ESCAPE    (leave-line app now)
      sdl/K-RETURN    (run-line app now)
      sdl/K-KP-ENTER  (run-line app now)
      sdl/K-TAB       (assoc app :command (complete command) :dirty? true :blink-from now)
      ;; Backspacing past the `:` leaves the command line.
      sdl/K-BACKSPACE (if (empty? command)
                        (leave-line app now)
                        (-> app (assoc :command (subs command 0 (dec (count command))))
                            (assoc :dirty? true :blink-from now)))
      app)))

;; ---------------------------------------------------------------- hints

(def ^:private hint-gap "Points between the columns of command hints." 16)
(def ^:private hint-padding "Points above and below the command hints." 4)
(def ^:private hint-space "Points between the command hints and the status bar." 4)
(def ^:private hint-rows "The most rows of command hints shown." 2)

(defn- hint-columns
  "`names` in columns, read down each and then across, filling no more
  than `hint-rows` rows: on one row while they fit across `avail` pixels
  in columns `col-w` wide and `gap` apart, else on as many columns as fit,
  leaving out those that don't."
  [names col-w gap avail]
  (let [fit  (max 1 (quot (+ avail gap) (+ col-w gap)))
        rows (if (<= (count names) fit) 1 hint-rows)]
    (mapv vec (partition-all rows (take (* rows fit) names)))))

(defn hints
  "While the command line is open, a float across the window just above
  the status bar, listing the commands that what is typed begins,
  alphabetically, in line with the status bar's text; nil
  when it begins none."
  [app]
  (when (and (command? app) (not (:goto? app)))
    (let [[typed] (parse-command (:command app))
          names (names-beginning typed)]
      (when (seq names)
        (let [d       (:density app)
              [w]     (:size app)
              m       (px app (:margin app))
              col-w   (reduce max (map #(ui-width app %) names))
              columns (hint-columns names col-w (px app hint-gap) (- w (* 2 m)))
              label   (fn [name] {:kind :label :text name
                                  :style {:color (:status-foreground app)}})]
          ;; The side padding, with the border, is the margin: the names
          ;; line up with the command line's text.
          {:kind :box
           :style {:position :absolute :left 0 :right 0
                   :bottom (+ (/ (status-height app) d) hint-space)
                   :direction :row :gap hint-gap :border 1
                   :padding [hint-padding (- (:margin app) 1)]
                   :background (:status-background app) :border-color (:ui-border app)}
           :children (mapv (fn [col] {:kind :box :style {:width (/ col-w d)}
                                      :children (mapv label col)})
                           columns)})))))
