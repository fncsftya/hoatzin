(ns hoatzin.app.files
  "The file the text is of: writing the text to it, and whether the text
  has changed since. Opening one is hoatzin.app.buffers's: each file is
  visited in a buffer of its own.

  The host does the file system's part: it shows the dialogs and reads
  and writes the files (see the hoatzin.app ns doc)."
  (:require [clojure.string :as str]
            [hoatzin.app.insets :as insets]
            [hoatzin.app.modes :as modes]
            [hoatzin.lib.text :as text]))

(defn file-name [path] (some-> path (str/split #"/") peek))

(defn parent
  "The directory holding `path`."
  [path]
  (let [i (str/last-index-of path "/")]
    (cond (nil? i)  nil
          (zero? i) "/"
          :else     (subs path 0 i))))

(defn file-lines
  "How many lines `t` has, as an editor counts them: a final newline ends
  the last line rather than starting another."
  [t]
  (let [n (text/line-count t)]
    (if (and (> n 1) (= "" (text/line t (dec n)))) (dec n) n)))

(defn mark-saved
  "The app with its insets as they are noted as those in its file."
  [app]
  (assoc app :saved-insets (insets/snapshot app)))

(defn sync-modified
  "Whether the text, or its insets (see hoatzin.app.insets), differs from
  the file, compared only when either has changed since last time. Texts
  share the lines an edit left alone, which compare by identity, so even
  then this costs little."
  [{:keys [doc saved compared] :as app}]
  (let [text (:text doc)
        now  [text saved (:insets app) (:saved-insets app)]]
    (if (and compared (every? true? (map identical? now compared)))
      app
      (let [modified? (or (not= text saved) (not= (insets/snapshot app) (:saved-insets app [])))]
        (cond-> (assoc app :compared now :modified? modified?)
          (not= modified? (:modified? app)) (assoc :dirty? true))))))

(defn write-file
  "Write the text to `path`, which is then the file it is the text of,
  and whose directory is the buffer's. The buffer's mode writes it; a
  buffer in none takes the mode for `path`'s extension, if there is one."
  [app path]
  (let [t    (get-in app [:doc :text])
        file (file-name path)
        mode (or (:major-mode app) (modes/for-path app path))
        doc  {:text (str t) :insets (insets/snapshot app)}
        {s :text error :error} (modes/write-text app mode doc)
        error (or error ((:write-file-fn app) path s))]
    (assoc (if error
             (assoc app :message (str "Can't write " file ": " error))
             (cond-> (-> (assoc app :path path :dir (parent path) :saved t
                                :message (str "\"" file "\" " (file-lines t) " lines written"))
                         mark-saved)
               mode (assoc :major-mode mode)))
           :dirty? true)))

(defn dialog-dir
  "Where a file dialog starts: the directory the last one chose in, else
  the buffer's."
  [app]
  (or (:dialog-dir app) (:dir app)))

(defn chosen-in
  "The app with `dir` as the directory a file dialog last chose in."
  [app dir]
  (assoc app :dialog-dir dir))

(defn open
  "Ask for a file to open; the answer comes back as :opened."
  [app]
  ((:open-dialog-fn app) (dialog-dir app))
  app)

(defn save-as
  "Ask where to save, starting at the file, else where `dialog-dir` says;
  the answer comes back as :save-chosen."
  [app]
  ((:save-dialog-fn app) (or (:path app) (dialog-dir app)))
  app)

(defn save-chosen
  "Where to save, as a :save-chosen event has it: written there, or why not."
  [app {:keys [path error]}]
  (if error
    (assoc app :message (str "Can't save: " error) :dirty? true)
    (-> app (chosen-in (parent path)) (write-file path))))
