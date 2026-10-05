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
                                       alt, shift, cmd), as in \"cmd+s\"
     :commands   {\"name\" (fn [app now force? arg] app)}
                                       command line commands, over the
                                       editor's own: `force?` is a trailing
                                       `!`, `arg` what follows the name
     :help       [[title [[keys what] ...]] ...]
                                       sections of the help window (`?`),
                                       before the editor's own
     :inset-title \"Section\"           what an inset's header says}

  every key but :name optional. A doc is the text with its insets (see
  hoatzin.app.insets): {:text s :insets [{:after k :text s} ...]}, the
  insets in order down the text, each below paragraph :after (-1: above
  the first); :read may answer just the text, a string. :read and :write
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
  "The app with a new inset holding `s` (\"\" if not given), below the
  paragraph or inset the caret is in, and the caret in it."
  ([app now] (insets/add app now))
  ([app now s] (insets/add app now s)))

(defn- remove-inset
  "The app without inset `id`."
  [app now id]
  (insets/remove-inset app now id))

(defn- current-inset
  "The id of the inset the caret is in, or nil."
  [app]
  (:inset app))

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

(defn current
  "The current buffer's mode, or nil."
  [app]
  (some->> (:major-mode app) (get (:modes app))))

(defn for-path
  "The name of the mode for files like `path`, by its extension, or nil."
  [app path]
  (when-let [ext (some->> path (re-find #"\.([^./]+)$") second)]
    (some (fn [[name m]] (when (some #{ext} (:extensions m)) name))
          (sort-by key (:modes app)))))

(defn- inset-spec? [{:keys [after text]}]
  (and (integer? after) (>= after -1) (string? text)))

(defn- as-doc
  "What a mode's :read answered as a doc, its newlines normalized, or nil
  if it isn't one."
  [r]
  (cond
    (string? r) {:text (text/normalize-newlines r) :insets []}
    (and (map? r) (string? (:text r)) (every? inset-spec? (:insets r)))
    {:text (text/normalize-newlines (:text r))
     :insets (mapv #(update % :text text/normalize-newlines) (:insets r))}))

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
  "Mode function `f`, which takes the app and answers it: a mode's
  mistake, thrown or answering something else, leaves the app as it was
  and says what it was."
  [f]
  (fn [app & args]
    (let [mode (:major-mode app)]
      (try (let [app' (apply f app args)]
             (if (map? app')
               app'
               (message app (str mode " mode answered no app"))))
           (catch Exception e
             (message app (str (ex-message e) " (" mode " mode)")))))))

(defn normal-command
  "The current buffer's mode's normal mode command for `text`, or nil."
  [app text]
  (some-> (get-in (current app) [:normal text]) guarded))

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
  \"cmd+s\"; nil without a modifier, or for a key with no name."
  [key mod]
  (let [mods (keep (fn [[n m]] (when (pos? (bit-and mod m)) n)) modifiers)
        name (or (key-names key) (when (< 0x20 key 0x7f) (str (char key))))]
    (when (and name (seq mods)) (str/join "+" (concat mods [name])))))

(defn- canonical
  "Binding `s` with its modifiers in order, if it names a key with any."
  [s]
  (let [parts (str/split s #"\+")
        mods  (set (butlast parts))
        known (map first modifiers)]
    (when (and (> (count parts) 1) (every? (set known) mods))
      (str/join "+" (concat (filter mods known) [(last parts)])))))

(defn normal-key
  "The current buffer's mode's normal mode command for key `key` with
  modifiers `mod`, or nil."
  [app key mod]
  (when-let [c (chord key mod)]
    (some (fn [[k f]] (when (= c (canonical k)) (guarded f))) (:normal (current app)))))

(defn help
  "The current buffer's mode's sections of the help window."
  [app]
  (:help (current app)))

(defn commands
  "The current buffer's mode's command line commands, by name."
  [app]
  (update-vals (:commands (current app)) guarded))

(defn switch
  "Put the current buffer in mode `name`; \"text\", or nothing, takes it
  out of any. Without a name, says what mode it is in, and which there are."
  [app name]
  (let [app (assoc app :dirty? true)]
    (cond
      (str/blank? name)
      (assoc app :message (str "Mode " (or (:major-mode app) "text") "; modes: "
                               (str/join ", " (cons "text" (sort (keys (:modes app)))))))

      (= "text" name)
      (-> app (dissoc :major-mode) (assoc :message "Text mode"))

      (contains? (:modes app) name)
      (assoc app :major-mode name :message (str (str/capitalize name) " mode"))

      :else
      (assoc app :message (str "No such mode: " name)))))
