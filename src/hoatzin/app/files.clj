(ns hoatzin.app.files
  "The file the text is of: opening one, writing the text to it, and
  whether the text has changed since.

  The host does the file system's part: it shows the dialogs and reads
  and writes the files (see the hoatzin.app ns doc)."
  (:refer-clojure :exclude [load-file])
  (:require [clojure.string :as str]
            [hoatzin.app.history :as history]
            [hoatzin.app.state :refer [touched]]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.text :as text]))

(defn file-name [path] (some-> path (str/split #"/") peek))

(defn- file-lines
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

(defn load-file
  "The file chosen to open, as an :opened event has it: loaded with the
  caret at its start, or why not."
  [app now {:keys [path text error]}]
  (let [file (file-name path)]
    (if error
      (assoc app :message (str "Can't open " (or file "a file") ": " error) :dirty? true)
      (let [t (text/of (text/normalize-newlines text))]
        (-> app
            (assoc :doc (assoc ed/empty-doc :text t)
                   :path path :saved t :scroll 0 :goal-x nil :upstream? false
                   :blocks {}
                   :message (str "\"" file "\" " (file-lines t) " lines"))
            (dissoc :composition :dragging? :drag-word :drag-point)
            history/reset
            (touched now))))))

(defn write-file
  "Write the text to `path`, which is then the file it is the text of."
  [app path]
  (let [t (get-in app [:doc :text])
        file (file-name path)]
    (assoc (if-let [error ((:write-file-fn app) path (str t))]
             (assoc app :message (str "Can't write " file ": " error))
             (assoc app :path path :saved t
                        :message (str "\"" file "\" " (file-lines t) " lines written")))
           :dirty? true)))

(defn save-as
  "Ask where to save; the answer comes back as :save-chosen."
  [app]
  ((:save-dialog-fn app) (:path app))
  app)

(defn save-chosen
  "Where to save, as a :save-chosen event has it: written there, or why not."
  [app {:keys [path error]}]
  (if error
    (assoc app :message (str "Can't save: " error) :dirty? true)
    (write-file app path)))
