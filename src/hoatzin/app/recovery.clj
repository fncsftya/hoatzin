(ns hoatzin.app.recovery
  "The host's part of crash recovery: keeping what hoatzin.app.journal asks
  it to, and reading it back, after a crash, from a session that is not
  running any more.

  It is all in the recovery directory of the hoatzin directory of the XDG
  config home (~/.config unless $XDG_CONFIG_HOME says otherwise). Each
  running editor has a session directory there, named for when it began
  and its process, holding its pid and, for each buffer with changes
  to lose, n being the buffer's number:
    b<n>.base   the buffer as it was, as EDN: {:gen :meta :text :insets :caret}
    b<n>.wal    the log of edits since: a line of EDN each, the first
                [:gen g], g being the base's generation, the rest
                [:t lo old-count new caret] and [:i insets]
  A base is written whole to a file of its own and moved into place, so one
  is never half written; then the log begins anew. A log of another
  generation than its base's, which a crash between the two leaves, is
  ignored. A crash may leave the last line of a log half written, and
  reading stops at the first line it cannot read. Nothing is flushed to
  the disk's platter: it is kept from the editor crashing, or being
  killed, not from the machine losing power.

  A session whose process has gone, and that has anything in it, is what
  `sessions` finds to recover; once recovered (and kept anew by the
  session that did it), `discard!` removes it."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [hoatzin.lib.text :as text]))

(defn root
  "Where the sessions are."
  []
  (str (fs/path (fs/xdg-config-home "hoatzin") "recovery")))

(defn- pid [] (.pid (ProcessHandle/current)))

(defn- alive?
  "Whether the process `pid` is running."
  [pid]
  (zero? (:exit (shell/sh "kill" "-0" (str pid)))))

;; ---------------------------------------------------------------- keeping

(defn start!
  "A new session in `root`: {:dir d}, or with :error, why it could not be."
  [root]
  (let [dir (str (fs/path root (str (System/currentTimeMillis) "-" (pid))))]
    (try (fs/create-dirs dir)
         (spit (str (fs/path dir "pid")) (str (pid)))
         {:dir dir}
         (catch Exception e {:dir dir :error (ex-message e)}))))

(defn- write-atomically! [path s]
  (let [tmp (str path ".tmp")]
    (spit tmp s)
    (fs/move tmp path {:replace-existing true})))

(defn- apply-op! [dir {:keys [op id gen] :as o}]
  (let [base (str (fs/path dir (str "b" id ".base")))
        wal  (str (fs/path dir (str "b" id ".wal")))
        line #(spit wal (str (pr-str %) "\n") :append true)]
    (case op
      :base    (do (fs/create-dirs dir)
                   (write-atomically! base (str (pr-str (select-keys o [:gen :meta :text :insets :caret])) "\n"))
                   (spit wal (str (pr-str [:gen gen]) "\n")))
      :edit    (line [:t (:lo o) (:old-count o) (:new o) (:caret o)])
      :insets  (line [:i (:insets o)])
      :discard (do (fs/delete-if-exists base) (fs/delete-if-exists wal)))))

(defn apply-ops!
  "Keep `ops` (see hoatzin.app.journal) in `session`: nil, or why not."
  [{:keys [dir error]} ops]
  (or error
      (try (run! #(apply-op! dir %) ops)
           nil
           (catch Exception e (or (ex-message e) (str e))))))

(defn finish!
  "End `session`: its files are removed, unless `keep?`, because there is
  work in them that would be lost, to be recovered the next time."
  [{:keys [dir]} keep?]
  (when-not keep?
    (try (fs/delete-tree dir) (catch Exception _ nil))))

;; ---------------------------------------------------------------- recovering

(defn- apply-line
  "`buffer` with log line `l` made, or nil if it can't be."
  [buffer [kind & args]]
  (case kind
    :t (let [[lo n new caret] args
             t (:text buffer)]
         (when (and (integer? lo) (integer? n) (string? new) (<= 0 lo) (<= (+ lo n) (count t)))
           (assoc buffer :text (text/replace t lo (+ lo n) new) :caret caret)))
    :i (assoc buffer :insets (first args))
    nil))

(defn- read-buffer
  "The buffer in base file `base`, with its log's edits made: {:id :meta
  :text :insets :caret}."
  [base]
  (let [wal   (str/replace base #"\.base$" ".wal")
        b     (edn/read-string (slurp base))
        lines (when (fs/exists? wal) (str/split-lines (slurp wal)))
        ok?   (and (seq lines) (= [:gen (:gen b)] (try (edn/read-string (first lines))
                                                       (catch Exception _ nil))))
        b     (assoc b :text (text/of (:text b)))
        b     (if ok?
                (loop [b b, ls (rest lines)]
                  (let [b' (when (seq ls)
                             (try (apply-line b (edn/read-string (first ls)))
                                  (catch Exception _ nil)))]
                    (if b' (recur b' (rest ls)) b)))
                b)]
    (-> (select-keys b [:meta :text :insets :caret])
        (update :text str)
        (assoc :id (parse-long (re-find #"\d+" (str (fs/file-name base))))))))

(defn- read-session
  "{:dir d :buffers [...]} of a session, :errors holding why any buffer
  could not be read."
  [dir]
  (reduce (fn [acc base]
            (try (update acc :buffers conj (read-buffer (str base)))
                 (catch Exception e
                   (update acc :errors (fnil conj []) (str (fs/file-name base) ": " (ex-message e))))))
          {:dir (str dir) :buffers []}
          (sort (fs/glob dir "b*.base"))))

(defn sessions
  "The sessions left by editors that are not running now, oldest first, as
  {:dir d :buffers [{:id :meta :text :insets :caret}] :errors [s]}: those
  with nothing in them are removed, and those that can't be read are kept
  under another name, and reported in :errors."
  [root]
  (if-not (fs/directory? root)
    []
    (vec (keep (fn [dir]
                 (let [pidf (fs/path dir "pid")
                       p    (when (fs/exists? pidf) (parse-long (str/trim (slurp (str pidf)))))]
                   (when-not (and p (alive? p))
                     (let [{:keys [buffers errors] :as s} (read-session dir)]
                       (cond
                         errors (do (fs/move dir (str dir ".unreadable") {:replace-existing true})
                                    (assoc s :dir (str dir ".unreadable")))
                         (empty? buffers) (do (fs/delete-tree dir) nil)
                         :else s)))))
               (sort (filter #(and (fs/directory? %) (not (str/ends-with? (str %) ".unreadable")))
                             (fs/list-dir root)))))))

(defn discard!
  "Remove a session `sessions` found, once what is in it is kept anew,
  unless it had files that could not be read: they stay for the user."
  [{:keys [dir]}]
  (when-not (str/ends-with? dir ".unreadable")
    (try (fs/delete-tree dir) (catch Exception _ nil))))
