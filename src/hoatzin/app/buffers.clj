(ns hoatzin.app.buffers
  "Buffers, after Emacs: every text being edited is a buffer, and one of
  them, the current buffer, is shown. A session starts with the scratch
  buffer, which visits no file; opening a file visits it in a buffer of its
  own, or switches to the buffer already visiting it, and `:new` starts an
  empty one.

  The current buffer's state is where the rest of the app works on it: at
  the top of the app map, under `buffer-keys`. The app's :buffers is every
  buffer, in the order they were made, each a map of those keys; the
  current one's entry is only brought up to date (`stash`ed) as the app
  switches away from it, or lists them.

  Each buffer has a directory, :dir, as Emacs's default-directory: the
  directory of the file it visits, that of the buffer it was made from, or
  where `:cd` chose. The current buffer's is the working directory. The
  file dialogs start where the last one chose, whichever buffer it was
  for (see hoatzin.app.files/dialog-dir), and in the working directory
  until one has."
  (:require [hoatzin.app.files :as files]
            [hoatzin.app.history :as history]
            [hoatzin.app.state :refer [touched]]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.text :as text]))

(def buffer-keys
  "What the app holds of the current buffer, at its top level."
  [:buffer-id :buffer-name :scratch? :path :dir :doc :saved :modified? :compared
   :undo :undo-tail :undo-chain :scroll :goal-x :upstream? :blocks])

(def ^:private passing-keys
  "What belongs to the moment rather than to a buffer, left behind as the
  app switches to another."
  [:composition :dragging? :drag-word :drag-point :grab
   :scroll-target :scroll-pos :scroll-at])

(def scratch-name "scratch")
(def new-name "untitled")

(defn- fresh
  "A new, empty buffer, numbered `id`, with `kvs` (see `buffer-keys`)."
  [id kvs]
  (merge {:buffer-id id :doc ed/empty-doc :saved (:text ed/empty-doc) :modified? false
          :scroll 0 :blocks {} :goal-x nil :upstream? false}
         kvs))

(defn init
  "The app with the scratch buffer as its only buffer, in directory `dir`."
  [app dir]
  (let [b (fresh 0 {:buffer-name scratch-name :scratch? true :dir dir})]
    (merge (apply dissoc app buffer-keys)
           b
           {:buffers [b] :next-buffer-id 1})))

;; ---------------------------------------------------------------- the list

(defn stash
  "The app with the current buffer's entry in :buffers up to date."
  [app]
  (let [app (files/sync-modified app)
        id  (:buffer-id app)
        b   (select-keys app buffer-keys)]
    (assoc app :buffers (mapv #(if (= id (:buffer-id %)) b %) (:buffers app)))))

(defn listing
  "Every buffer, up to date, in the order they were made."
  [app]
  (:buffers (stash app)))

(defn unsaved?
  "Whether buffer `b` has changes that would be lost: the scratch buffer's
  are only its own, unless it has been saved to a file."
  [b]
  (boolean (and (:modified? b) (or (:path b) (not (:scratch? b))))))

(defn names
  "The names buffers `bs` are shown by, by :buffer-id: the name of the file
  each visits, else its own, and for the second and later buffers of the
  same name, <n> after it, as Emacs does."
  [bs]
  (first (reduce (fn [[out seen] b]
                   (let [base (or (files/file-name (:path b)) (:buffer-name b))
                         n    (inc (seen base 0))]
                     [(assoc out (:buffer-id b) (if (= 1 n) base (str base "<" n ">")))
                      (assoc seen base n)]))
                 [{} {}]
                 bs)))

(defn buffer-name
  "The name the current buffer is shown by."
  [app]
  ((names (listing app)) (:buffer-id app)))

;; ---------------------------------------------------------------- switching

(defn- show
  "Make buffer `b` the current buffer, leaving the one that was to be
  stashed, or forgotten, beforehand."
  [app now b]
  (-> (apply dissoc app (concat buffer-keys passing-keys))
      (merge b)
      (assoc :dirty? true :blink-from now)))

(defn switch
  "Make the buffer numbered `id` the current buffer."
  [app now id]
  (if (= id (:buffer-id app))
    app
    (let [app (stash app)]
      (if-let [b (some #(when (= id (:buffer-id %)) %) (:buffers app))]
        (show app now b)
        app))))

(defn- add
  "Make a new buffer of `kvs` (see `buffer-keys`), after the others, and
  switch to it."
  [app now kvs]
  (let [app (stash app)
        b   (fresh (:next-buffer-id app) kvs)]
    (-> app
        (update :buffers conj b)
        (update :next-buffer-id inc)
        (show now b)
        (touched now))))

(defn new-buffer
  "Start an empty buffer, in the current buffer's directory."
  [app now]
  (add app now {:buffer-name new-name :dir (:dir app)}))

(defn open-file
  "The file chosen to open, as an :opened event has it: switched to, in
  the buffer visiting it already, else visited in a new one with the
  caret at its start; or why it could not be. Either way, the next file
  dialog starts in the directory it was chosen in."
  [app now {:keys [path text error]}]
  (let [file (files/file-name path)
        app  (cond-> app path (files/chosen-in (files/parent path)))]
    (cond
      error
      (assoc app :message (str "Can't open " (or file "a file") ": " error) :dirty? true)

      (some #(= path (:path %)) (listing app))
      (switch app now (some #(when (= path (:path %)) (:buffer-id %)) (listing app)))

      :else
      (let [t (text/of (text/normalize-newlines text))]
        (-> (add app now {:path path :dir (files/parent path) :saved t
                          :doc (assoc ed/empty-doc :text t)})
            (assoc :message (str "\"" file "\" " (files/file-lines t) " lines")))))))

(defn close
  "Close the current buffer, unless it has unsaved changes and not
  `force?`, and switch to the one before it (or, for the first, after it).
  Closing the only buffer leaves a new scratch buffer in its place."
  [app now force?]
  (let [app  (stash app)
        name (buffer-name app)]
    (if (and (unsaved? app) (not force?))
      (assoc app :message (str "\"" name "\" has unsaved changes (add ! to override)") :dirty? true)
      (let [bs   (:buffers app)
            i    (first (keep-indexed #(when (= (:buffer-id app) (:buffer-id %2)) %1) bs))
            left (into (subvec bs 0 i) (subvec bs (inc i)))
            app  (assoc app :buffers left :message (str "Closed \"" name "\""))]
        (if (seq left)
          (show app now (left (max 0 (dec i))))
          (let [b (fresh (:next-buffer-id app) {:buffer-name scratch-name :scratch? true
                                                :dir (:dir app)})]
            (-> app (assoc :buffers [b]) (update :next-buffer-id inc) (show now b))))))))

(defn revert
  "Read the current buffer's file again, replacing its text; undo
  restores what it was. A buffer visiting no file has nothing to revert
  to, and says so."
  [app now]
  (if-let [path (:path app)]
    (let [{:keys [text error]} ((:read-file-fn app) path)
          file (files/file-name path)]
      (if error
        (assoc app :message (str "Can't revert " file ": " error) :dirty? true)
        (let [old (:doc app)
              t   (text/of (text/normalize-newlines text))
              s   (str t)
              doc (-> old (dissoc :anchor) (ed/delete 0 (count (:text old)))
                      (ed/move 0) (ed/insert s))
              doc (ed/move doc (min (:caret old) (count s)))]
          (-> app
              (dissoc :undo-tail)
              (assoc :doc doc :saved (:text doc) :goal-x nil :upstream? false
                     :message (str "Reverted \"" file "\" " (files/file-lines t) " lines"))
              (history/record old 0 (count (:text old)) s)
              (touched now)))))
    (assoc app :message (str "\"" (buffer-name app) "\" has no file to revert to") :dirty? true)))

(defn cd
  "Ask for a directory to change to; the answer comes back as :dir-chosen."
  [app]
  ((:dir-dialog-fn app) (:dir app))
  app)

(defn dir-chosen
  "The directory chosen, as a :dir-chosen event has it: the current
  buffer's now, and where the next file dialog starts; or why not."
  [app {:keys [path error]}]
  (assoc (if error
           (assoc app :message (str "Can't change directory: " error))
           (-> (files/chosen-in app path) (assoc :dir path :message (str "Directory " path))))
         :dirty? true))
