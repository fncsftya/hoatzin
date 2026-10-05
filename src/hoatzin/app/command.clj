(ns hoatzin.app.command
  "The command line: `:` in normal mode opens it in the status bar,
  return runs it and escape abandons it. While it is open, a box above
  the status bar hints at the commands what is typed could still be.

  A command runs from any prefix that begins no other, so `:w` is
  `:write`, and tab completes as far as the commands it could be agree.
  What follows the command's name, after a space, is its argument, as in
  `:mode auk`; for `:mode` and `:minor`, tab completes it too, from the
  modes' names, and the hints list those it could be. The current buffer's mode (see hoatzin.app.modes) may add
  commands, or replace the editor's own."
  (:require [clojure.string :as str]
            [hoatzin.app.buffers :as buffers]
            [hoatzin.app.buffers-window :as buffers-window]
            [hoatzin.app.files :as files]
            [hoatzin.app.hints :as hints]
            [hoatzin.app.insets :as insets]
            [hoatzin.app.modes :as modes]
            [hoatzin.app.search :as search]
            [hoatzin.app.settings-window :as settings-window]
            [hoatzin.app.state :refer [command? enter-mode touched]]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.text :as text]
            [hoatzin.lib.sdl :as sdl]))

(defn line-text
  "The command line as shown: its text after the `:`, or after the prompt
  for a line number, or the search prompt."
  [app]
  (if (:search? app)
    (search/prompt-text app)
    (str (if (:goto? app) "Go to line: " ":") (:command app))))

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

;; Each command is (fn [app now force? arg]), `force?` being a trailing `!`
;; and `arg` what follows the name, or "".
(def ^:private builtin-commands
  {"open"  (fn [app _ _ _] (files/open app))
   "write" (fn [app _ _ _] (if-let [path (:path app)] (files/write-file app path) (files/save-as app)))
   "save"  (fn [app _ _ _] (files/save-as app))
   "quit"  (fn [app now force? _] (quit app now force?))
   "buffers" (fn [app _ _ _] (buffers-window/open app))
   "new"   (fn [app now _ _] (buffers/new-buffer app now))
   "close" (fn [app now force? _] (buffers/close app now force?))
   "revert" (fn [app now _ _] (buffers/revert app now))
   "cd"    (fn [app _ _ _] (buffers/cd app))
   "mode"  (fn [app _ _ arg] (modes/switch app arg))
   "minor" (fn [app _ _ arg] (modes/toggle-minor app arg))
   "settings" (fn [app _ _ _] (settings-window/open app))})

(defn- commands
  "The commands, by name: the editor's, and its mode's over them."
  [app]
  (merge builtin-commands (modes/commands app)))

(defn- names-beginning
  "The commands' names that begin with `typed`, alphabetically."
  [app typed]
  (filterv #(str/starts-with? % typed) (sort (keys (commands app)))))

(defn- command-names
  "The commands `typed` could mean: the one it names, else those it begins."
  [app typed]
  (if (contains? (commands app) typed)
    [typed]
    (names-beginning app typed)))

(defn- parse-command
  "The command line as [command force? arg]: the name, `force?` being a
  `!` after it, and what follows it after a space."
  [line]
  (let [[typed arg] (str/split (str/trim line) #"\s+" 2)
        force? (str/ends-with? typed "!")]
    [(cond-> typed force? (subs 0 (dec (count typed)))) force? (or arg "")]))

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
  (-> app (enter-mode now :normal) (dissoc :command :goto? :search?)))

(defn- jump
  "Move the caret to `pos` in the document, leaving a mark where it was
  to jump back to with `j`."
  [app now pos]
  (-> (insets/leave app now)
      (assoc :doc (-> (:doc app) (ed/move (:caret (:doc app))) ed/set-mark (ed/move pos))
             :goal-x nil :upstream? false)
      (dissoc :selecting?)
      (touched now)))

(defn- goto-line
  "Move the caret to the start of the 1-based line typed, clamped to the
  text's lines, or to the start (`s`) or end (`e`) of the buffer; anything
  else leaves it where it is."
  [app now]
  (let [typed (str/trim (:command app))]
    (case typed
      "s" (jump app now 0)
      "e" (jump app now (count (:text (:doc app))))
      (if-let [n (parse-long typed)]
        (let [t (text/of (get-in app [:doc :text]))
              k (-> n dec (max 0) (min (dec (text/line-count t))))]
          (jump app now (text/line-start t k)))
        app))))

(declare run-command)

(defn- run-line
  "Run the command line, back in normal mode."
  [app now]
  (if (:goto? app)
    (leave-line (goto-line app now) now)
    (run-command app now)))

(defn- run-command [app now]
  (let [[command force? arg] (parse-command (:command app))
        app     (leave-line app now)
        names   (command-names app command)]
    (cond
      (= "" command)     app
      (= 1 (count names)) (((commands app) (first names)) app now force? arg)
      (seq names)        (assoc app :message (str "Ambiguous command: " command
                                                  " (" (str/join ", " names) ")"))
      :else              (assoc app :message (str "Not an editor command: " command)))))

(defn- shared-start [a b]
  (subs a 0 (count (take-while true? (map = a b)))))

(defn- arguments
  "What the argument of command `name` may be, for completing it, or nil
  if it could be anything."
  [app name]
  (case name
    "mode"  (modes/major-names app)
    "minor" (modes/minor-names app)
    nil))

(defn- argument-split
  "Command line `command` as [what is before its argument, its
  argument], or nil if it has none yet."
  [command]
  (when-let [[_ head arg] (re-find #"^(\s*\S+\s+)(.*)$" command)]
    [head arg]))

(defn- arguments-beginning
  "The arguments the command on command line `command` may take that
  begin what is typed of its argument, or nil if it has none yet, or
  the command could be anything or takes anything."
  [app command]
  (when-let [[_ arg] (argument-split command)]
    (let [[typed] (parse-command command)
          names   (command-names app typed)]
      (when (= 1 (count names))
        (seq (filter #(str/starts-with? % arg) (arguments app (first names))))))))

(defn- complete
  "The command line completed as far as the commands it could mean agree;
  once it has an argument, as far as the arguments it could be agree."
  [app command]
  (if-let [[head] (argument-split command)]
    (if-let [args (arguments-beginning app command)]
      (str head (reduce shared-start args))
      command)
    (let [names (command-names app (str/triml command))]
      (if (seq names) (reduce shared-start names) command))))

(defn on-text
  "Typed text, onto the end of the command line (or the search prompt)."
  [app now text]
  (if (:search? app)
    (search/on-text app now text)
    (if (:goto? app)
      (let [lower (str/lower-case text)
            digits (apply str (filter #(Character/isDigit ^char %) text))]
        (if (and (empty? (:command app)) (#{"s" "e"} lower))
          ;; `s` and `e` jump at once, when they are all that is typed
          (leave-line (goto-line (assoc app :command lower) now) now)
          (-> app (update :command str digits) (assoc :dirty? true :blink-from now))))
      (-> app (update :command str text) (assoc :dirty? true :blink-from now)))))

(declare on-command-key)

(defn on-key
  "A key on the command line. Other keys leave the document alone."
  [app now key]
  (if (:search? app)
    (search/on-key app now key)
    (on-command-key app now key)))

(defn- on-command-key
  "A key on the command line."
  [app now key]
  (let [command (:command app)]
    (condp = key
      sdl/K-ESCAPE    (leave-line app now)
      sdl/K-RETURN    (run-line app now)
      sdl/K-KP-ENTER  (run-line app now)
      sdl/K-TAB       (assoc app :command (complete app command) :dirty? true :blink-from now)
      ;; Backspacing past the `:` leaves the command line.
      sdl/K-BACKSPACE (if (empty? command)
                        (leave-line app now)
                        (-> app (assoc :command (subs command 0 (dec (count command))))
                            (assoc :dirty? true :blink-from now)))
      app)))

(defn hints
  "While the command line is open, a float across the window just above
  the status bar, listing the commands that what is typed begins, or,
  once it has an argument, the arguments it could be,
  alphabetically, in line with the status bar's text; nil
  when it begins none."
  [app]
  (when (and (command? app) (not (:goto? app)) (not (:search? app)))
    (let [[typed] (parse-command (:command app))
          ;; once there is an argument, the command is chosen
          names (if (argument-split (:command app))
                  (arguments-beginning app (:command app))
                  (names-beginning app typed))]
      (when (seq names)
        (hints/box app (map (fn [name] {:text name :node {:kind :label :text name
                                                          :style {:color (:foreground app)}}})
                            names))))))
