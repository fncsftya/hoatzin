(ns hoatzin.app.modes
  "Modes, after Emacs's major modes: what a kind of text is, written
  outside the editor and evaluated by SCI, the Small Clojure Interpreter.
  Each buffer is in at most one mode, its :major-mode, chosen by the
  extension of the file it visits as it is opened or saved, or by `:mode`.

  A mode is a file of Clojure whose last form is the mode, a map:

    {:name       \"auk\"                 what `:mode` calls it
     :extensions [\"auk\"]               the files it is for
     :read       (fn [s] doc)            a file's contents as the text to edit
     :write      (fn [doc] s)            the text as the file's contents
     :normal     {\"x\" (fn [app now] app)
                  \"cmd+s\" (fn [app now] app)}
                                       normal mode commands, over the
                                       editor's own: by the text typed, or
                                       by the key and its modifiers (ctrl,
                                       alt, shift, cmd), as in \"cmd+s\";
                                       a named key, as \"tab\" or
                                       \"return\", needs none. One that
                                       answers nil leaves the key to the
                                       editor
     :insert     {\"cmd+l\" (fn [app now] app)}
                                       insert mode commands, by the key and
                                       its modifiers, as :normal names them
     :commands   {\"name\" (fn [app now force? arg] app)}
                                       command line commands, over the
                                       editor's own: `force?` is a trailing
                                       `!`, `arg` what follows the name
     :help       [[title [[keys what] ...]] ...]
                                       sections of the help window (`?`),
                                       before the editor's own
     :inset-title \"Section\"           what an inset's header says}

  every key but :name optional.

  Minor modes, after Emacs's, are modes too, but a buffer may be in any
  number of them at once, its :minor-modes, each turned on and off with
  `:minor`. A minor mode is a map as a mode is, of :name, :normal,
  :insert, :commands and :help, and also
    :minor?     true                  what makes it a minor mode
    :default?   true                  whether each new buffer is in it
  and, for the editor's own, which work on the editor beyond what
  hoatzin.mode gives:
    :opened     (fn [app] app)        the buffer has visited its file
    :written    (fn [app] app)        the buffer has written its file
    :loaded     (fn [app event] app)  a :mode-data event for it: what it
                                      asked the host to load, as the
                                      hoatzin.app ns doc says
    :on-event   (fn [app now event] app)
                                      an event, before the text has it; nil
                                      leaves it to the text
    :status     (fn [app] s)          what the status bar says, or nil
    :draw-under (fn [app k0 k1])      draw behind the text's visual lines
                                      [k0, k1)
    :draw       (fn [app k0 k1])      draw over them
  A minor mode's keys and commands come before the buffer's mode's, and
  one that answers nil leaves the key to the next. The editor's own minor
  modes are hoatzin.app.variants.

  A doc is the text with its insets (see
  hoatzin.app.insets): {:text s :insets [{:after k :text s :insets [...]}
  ...]}, the insets in order down the text, each below paragraph :after
  (-1: above the first) and each with its own insets, as a doc has them,
  and with its :kind (:section, :list or :checklist), :title and, for a
  checklist, :checked, a tick for each paragraph, if it has them. :read
  may answer just the text, a string, and leave out an inset's :insets. :read and :write
  may throw to say the file can't be: what they throw says why. A mode
  without them reads and writes the text as it is, and can't write
  insets.

  Besides SCI's own namespaces (clojure.string, clojure.edn and so on),
  modes can use hoatzin.mode (see `api`). The editor's own modes are in
  resources/modes; the host may give more, from the user's config."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hoatzin.app.confirm :as confirm]
            [hoatzin.app.insets :as insets]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.text :as text]
            [sci.core :as sci]))

(def builtin
  "The editor's own modes, by their resource under modes/."
  ["auk"])

;; ---------------------------------------------------------------- the API

(def ^:private api-ns (sci/create-ns 'hoatzin.mode))

(defn- text
  "The current buffer's text."
  [app]
  (str (get-in app [:doc :text])))

(defn- message
  "The app saying `s` in the status bar, until the next keystroke."
  [app s]
  (assoc app :message s :dirty? true))

(declare guarded)

(defn- add-inset
  "The app with a new inset of `spec`, {:text s :kind k :title s :checked
  [...]}, or of string `spec` (\"\" if not given), below the paragraph
  the caret is in, in the innermost text it is in that can hold one (the
  buffer's or a section's), with a new line after it, and the caret in
  it."
  ([app now] (insets/add app now ""))
  ([app now spec] (insets/add app now spec)))

(defn- inset-info
  "What the inset at `path` is: {:kind :title :folded?}, or nil."
  [app path]
  (when-let [i (insets/inset-at app path)]
    {:kind (insets/kind i) :title (:title i) :folded? (boolean (:collapsed? i))}))

(defn- toggle-inset
  "Fold the section at `path`, or unfold it; folding the one the caret is
  in leaves the caret over it, and unfolding the one it is over puts the
  caret inside."
  [app now path]
  (insets/toggle-at app now path))

(defn- toggle-check
  "Tick, or untick, the item the caret is in of the checklist at `path`."
  [app now path]
  (insets/toggle-check app now path))

(defn- rename-inset
  "Begin renaming the section at `path`, in its header."
  [app path]
  (insets/start-rename app path))

(defn- cycle-indent
  "Indent the list item the caret is in, then take it out, then put it
  back, as tab does again and again; nil if it isn't in a list."
  [app now]
  (insets/cycle-indent app now))

(defn- list-return
  "Leave the list the caret is in (`all?`: every list) for a new line
  after it, if it is on the empty last item (`all?`: on any); else nil."
  [app now all?]
  (insets/list-return app now all?))

(defn- remove-inset
  "The app without the inset at `path`, as `current-inset` gives it."
  [app now path]
  (insets/remove-inset app now path))

(defn- current-inset
  "The inset the caret is in, as the path of ids down to it, or nil."
  [app]
  (insets/path app))

(defn- ask
  "Ask `prompt` in the status bar: answered yes, the app becomes
  (`yes` app now)."
  [app prompt yes]
  (confirm/ask app prompt (guarded yes)))

(def ^:private api
  "What modes may use of the editor, as the namespace hoatzin.mode."
  {'text          (sci/copy-var text api-ns)
   'message       (sci/copy-var message api-ns)
   'add-inset     (sci/copy-var add-inset api-ns)
   'remove-inset  (sci/copy-var remove-inset api-ns)
   'current-inset (sci/copy-var current-inset api-ns)
   'inset-info    (sci/copy-var inset-info api-ns)
   'toggle-inset  (sci/copy-var toggle-inset api-ns)
   'toggle-check  (sci/copy-var toggle-check api-ns)
   'rename-inset  (sci/copy-var rename-inset api-ns)
   'cycle-indent  (sci/copy-var cycle-indent api-ns)
   'list-return   (sci/copy-var list-return api-ns)
   'confirm       (sci/copy-var ask api-ns)})

(defn- context [] (sci/init {:namespaces {'hoatzin.mode api}}))

;; ---------------------------------------------------------------- loading

(defn- check
  "Mode `m`, as loaded from `origin`, if it is one; else throws why not."
  [origin m]
  (when-not (and (map? m) (string? (:name m)) (not (str/blank? (:name m))))
    (throw (ex-info (str origin ": not a mode, a map with a :name") {})))
  m)

(defn load-sources
  "{:modes {name mode} :errors [s]} from `sources`, [origin source] pairs,
  each evaluated in a context of its own; `:errors` says why each that
  could not be loaded wasn't. A later mode of the same name replaces an
  earlier one."
  [sources]
  (reduce (fn [acc [origin src]]
            (try (let [m (check origin (sci/eval-string* (context) src))]
                   (assoc-in acc [:modes (:name m)] m))
                 (catch Exception e
                   (update acc :errors conj (str "Can't load mode " origin ": " (ex-message e))))))
          {:modes {} :errors []}
          sources))

(defn builtin-sources
  "The editor's own modes, as [origin source] pairs."
  []
  (for [name builtin]
    [name (slurp (io/resource (str "modes/" name ".clj")))]))

;; ---------------------------------------------------------------- the buffer's

(defn minor? [m] (boolean (:minor? m)))

(defn current
  "The current buffer's mode, or nil."
  [app]
  (some->> (:major-mode app) (get (:modes app))))

(defn minors
  "The minor modes the current buffer is in, by name."
  [app]
  (keep #(get (:modes app) %) (sort (:minor-modes app))))

(defn default-minors
  "The names of the minor modes a new buffer is in."
  [app]
  (into #{} (keep (fn [[name m]] (when (and (minor? m) (:default? m)) name))) (:modes app)))

(defn for-path
  "The name of the mode for files like `path`, by its extension, or nil."
  [app path]
  (when-let [ext (some->> path (re-find #"\.([^./]+)$") second)]
    (some (fn [[name m]] (when (and (not (minor? m)) (some #{ext} (:extensions m))) name))
          (sort-by key (:modes app)))))

(defn- inset-spec?
  "Whether `i` is an inset of a doc: below a paragraph (or -1), of a kind
  there is, and a doc itself."
  [i]
  (and (integer? (:after i)) (>= (:after i) -1)
       (contains? #{nil :section :list :checklist} (:kind i))
       ((some-fn nil? string?) (:title i))
       ((some-fn nil? #(every? boolean? %)) (:checked i))
       (map? i) (string? (:text i))
       (every? inset-spec? (:insets i))))

(defn- doc?
  "Whether `d` is a doc: its insets, if any, insets of a doc."
  [d]
  (and (map? d) (string? (:text d)) (every? inset-spec? (:insets d))))

(defn- normalized
  "Doc `d` with its newlines, and its insets', normalized."
  [d]
  (-> d
      (update :text text/normalize-newlines)
      (assoc :insets (mapv normalized (:insets d)))))

(defn- as-doc
  "What a mode's :read answered as a doc, its newlines normalized, or nil
  if it isn't one."
  [r]
  (cond
    (string? r) {:text (text/normalize-newlines r) :insets []}
    (doc? r)    (normalized r)))

(defn read-text
  "File contents `s` as the doc of mode `name`'s buffer, {:text t :insets
  [...]}, or {:error e}, why it could not be."
  [app name s]
  (if-let [f (:read (get (:modes app) name))]
    (try (or (as-doc (f s)) {:error (str name " mode read no text")})
         (catch Exception e {:error (str (ex-message e) " (" name " mode)")}))
    {:text s :insets []}))

(defn write-text
  "`doc`, {:text t :insets [...]}, as the contents of mode `name`'s file:
  {:text s} or {:error e}, why it could not be."
  [app name doc]
  (if-let [f (:write (get (:modes app) name))]
    (try (let [s (f doc)]
           (if (string? s) {:text s} {:error (str name " mode wrote no text")}))
         (catch Exception e {:error (str (ex-message e) " (" name " mode)")}))
    (if (seq (:insets doc))
      {:error (str "only a mode can write its insets, and " (or name "text") " mode can't")}
      {:text (:text doc)})))

(defn- guarded
  "Mode function `f`, of the mode named `mode` (the buffer's, if not
  given), which takes the app and answers it, or nil to leave it to the
  editor: a mode's mistake, thrown or answering something else, leaves
  the app as it was and says what it was."
  ([f] (fn [app & args] (apply (guarded (:major-mode app) f) app args)))
  ([mode f]
   (fn [app & args]
     (try (let [app' (apply f app args)]
            (if (or (nil? app') (map? app'))
              app'
              (message app (str mode " mode answered no app"))))
          (catch Exception e
            (message app (str (ex-message e) " (" mode " mode)")))))))

(defn- in-turn
  "The mode functions `fs` as one, which answers what the first to answer
  anything does, or nil if none do; nil if there are none."
  [fs]
  (when (seq fs)
    (fn [app & args] (some #(apply % app args) fs))))

(defn- each-mode
  "The current buffer's minor modes, then its mode, as [name mode]."
  [app]
  (cond-> (mapv (juxt :name identity) (minors app))
    (current app) (conj [(:major-mode app) (current app)])))

(defn normal-command
  "The current buffer's modes' normal mode command for `text`, or nil."
  [app text]
  (in-turn (keep (fn [[name m]] (some->> (get-in m [:normal text]) (guarded name)))
                 (each-mode app))))

(def ^:private modifiers
  "The modifiers a key binding can name, in the order its name has them."
  [["ctrl" sdl/KMOD-CTRL] ["alt" sdl/KMOD-ALT] ["shift" sdl/KMOD-SHIFT] ["cmd" sdl/KMOD-GUI]])

(def ^:private key-names
  {sdl/K-RETURN "return" sdl/K-ESCAPE "escape" sdl/K-TAB "tab" sdl/K-BACKSPACE "backspace"
   sdl/K-DELETE "delete" sdl/K-UP "up" sdl/K-DOWN "down" sdl/K-LEFT "left" sdl/K-RIGHT "right"
   sdl/K-HOME "home" sdl/K-END "end" sdl/K-PAGEUP "pageup" sdl/K-PAGEDOWN "pagedown"
   0x20 "space"})

(defn- chord
  "Key `key` with modifiers `mod` named as a binding names it, as
  \"cmd+s\" or \"tab\"; nil for a key with no name, or a character's
  without a modifier, which is typed text."
  [key mod]
  (let [mods (keep (fn [[n m]] (when (pos? (bit-and mod m)) n)) modifiers)
        named (key-names key)
        name (or named (when (< 0x20 key 0x7f) (str (char key))))]
    (when (and name (or named (seq mods))) (str/join "+" (concat mods [name])))))

(defn- canonical
  "Binding `s` with its modifiers in order, if it names a key: with
  modifiers, or one of `key-names`."
  [s]
  (let [parts (str/split s #"\+")
        mods  (set (butlast parts))
        known (map first modifiers)]
    (when (and (every? (set known) mods)
               (or (> (count parts) 1) (contains? (set (vals key-names)) s)))
      (str/join "+" (concat (filter mods known) [(last parts)])))))

(defn- key-command
  "The command bound in mode `name`'s `bindings` to key `key` with
  modifiers `mod`, or nil."
  [name bindings key mod]
  (when-let [c (chord key mod)]
    (some (fn [[k f]] (when (= c (canonical k)) (guarded name f))) bindings)))

(defn- modes-key
  "The current buffer's modes' commands in `kind` (:normal or :insert)
  for key `key` with modifiers `mod`, as one, or nil."
  [app kind key mod]
  (in-turn (keep (fn [[name m]] (key-command name (kind m) key mod)) (each-mode app))))

(defn normal-key
  "The current buffer's modes' normal mode command for key `key` with
  modifiers `mod`, or nil."
  [app key mod]
  (modes-key app :normal key mod))

(defn insert-key
  "The current buffer's modes' insert mode command for key `key` with
  modifiers `mod`, or nil."
  [app key mod]
  (modes-key app :insert key mod))

(defn help
  "The current buffer's mode's sections of the help window."
  [app]
  (:help (current app)))

(defn minor-help
  "The current buffer's minor modes' sections of the help window."
  [app]
  (mapcat :help (minors app)))

(defn commands
  "The current buffer's modes' command line commands, by name: its
  minor modes' over its mode's."
  [app]
  (reduce (fn [cs [name m]] (merge (update-vals (:commands m) #(guarded name %)) cs))
          {} (each-mode app)))

;; ---------------------------------------------------------------- minor modes' hooks

(defn- run-hooks
  "The app after `hook` of each minor mode the buffer is in, in turn."
  [app hook & args]
  (reduce (fn [app m] (if-let [f (hook m)] (apply f app args) app)) app (minors app)))

(defn opened
  "The app after its minor modes have seen the buffer visit its file."
  [app]
  (run-hooks app :opened))

(defn written
  "The app after its minor modes have seen the buffer write its file."
  [app]
  (run-hooks app :written))

(defn loaded
  "The app after the minor mode a :mode-data `event` is for has it."
  [app event]
  (if-let [f (:loaded (get (:modes app) (:mode event)))]
    (f app event)
    app))

(defn on-event
  "The app after the first of the buffer's minor modes to take `event`
  has, or nil if none do."
  [app now event]
  (some #(some-> (:on-event %) (as-> f (f app now event))) (minors app)))

(defn status
  "What the buffer's minor modes have the status bar say, or nil."
  [app]
  (some #(some-> (:status %) (as-> f (f app))) (minors app)))

(defn draw-under!
  "Draw what the buffer's minor modes show behind its visual lines [k0,
  k1)."
  [app k0 k1]
  (doseq [m (minors app)]
    (some-> (:draw-under m) (as-> f (f app k0 k1)))))

(defn draw!
  "Draw what the buffer's minor modes show over its visual lines [k0, k1)."
  [app k0 k1]
  (doseq [m (minors app)]
    (some-> (:draw m) (as-> f (f app k0 k1)))))

(defn major-names
  "The names `:mode` takes: \"text\", and each mode's but the minor
  ones', alphabetically."
  [app]
  (cons "text" (sort (keep (fn [[n m]] (when-not (minor? m) n)) (:modes app)))))

(defn minor-names
  "The minor modes' names, alphabetically."
  [app]
  (sort (keep (fn [[n m]] (when (minor? m) n)) (:modes app))))

(defn toggle-minor
  "Turn minor mode `name` on in the current buffer, or off; it says
  which, and the hooks of one turned on see the buffer's file as if just
  visited. Without a name, says which the buffer is in, and which there
  are."
  [app name]
  (let [app (assoc app :dirty? true)
        all (minor-names app)]
    (cond
      (str/blank? name)
      (assoc app :message (str "Minor modes: "
                               (if (seq all)
                                 (str/join ", " (map #(str % (if (contains? (:minor-modes app) %) " (on)" " (off)"))
                                                     all))
                                 "none")))

      (not (minor? (get (:modes app) name)))
      (assoc app :message (str "No such minor mode: " name))

      (contains? (:minor-modes app) name)
      (-> app (update :minor-modes disj name) (assoc :message (str (str/capitalize name) " mode off")))

      :else
      (let [m   (get (:modes app) name)
            app (-> app (update :minor-modes (fnil conj #{}) name)
                    (assoc :message (str (str/capitalize name) " mode on")))]
        (if-let [f (and (:path app) (:opened m))] (f app) app)))))

(defn switch
  "Put the current buffer in mode `name`; \"text\", or nothing, takes it
  out of any. Without a name, says what mode it is in, and which there are."
  [app name]
  (let [app (assoc app :dirty? true)]
    (cond
      (str/blank? name)
      (assoc app :message (str "Mode " (or (:major-mode app) "text") "; modes: "
                               (str/join ", " (major-names app))))

      (minor? (get (:modes app) name))
      (assoc app :message (str name " is a minor mode: :minor " name " turns it on or off"))

      (= "text" name)
      (-> app (dissoc :major-mode) (assoc :message "Text mode"))

      (contains? (:modes app) name)
      (assoc app :major-mode name :message (str (str/capitalize name) " mode"))

      :else
      (assoc app :message (str "No such mode: " name)))))
