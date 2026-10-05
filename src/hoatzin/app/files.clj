(ns hoatzin.app.files
  "The file the text is of: writing the text to it, and whether the text
  has changed since. Opening one is hoatzin.app.buffers's: each file is
  visited in a buffer of its own.

  The host does the file system's part: it shows the dialogs and reads
  and writes the files (see the hoatzin.app ns doc)."
  (:require [clojure.string :as str]
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

(defn sync-modified
  "Whether the text differs from the file, compared only when either has
  changed since last time. Texts share the lines an edit left alone, which
  compare by identity, so even then this costs little."
  [{:keys [doc saved compared] :as app}]
  (let [text (:text doc)]
    (if (and (identical? text (first compared)) (identical? saved (second compared)))
      app
      (let [modified? (not= text saved)]
        (cond-> (assoc app :compared [text saved] :modified? modified?)
          (not= modified? (:modified? app)) (assoc :dirty? true))))))

(defn write-file
  "Write the text to `path`, which is then the file it is the text of,
  and whose directory is the buffer's."
  [app path]
  (let [t (get-in app [:doc :text])
        file (file-name path)]
    (assoc (if-let [error ((:write-file-fn app) path (str t))]
             (assoc app :message (str "Can't write " file ": " error))
             (assoc app :path path :dir (parent path) :saved t
                        :message (str "\"" file "\" " (file-lines t) " lines written")))
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
