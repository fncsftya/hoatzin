(ns hoatzin.app.journal
  "The write-ahead log of edits, so that a crash loses no work: what the
  app asks its host to keep (see hoatzin.app.recovery for the host's part,
  and the ns doc of hoatzin.app for :journal-fn).

  After every event, each buffer with changes not in its file is compared
  with what was last kept of it. The first time it differs, the host is
  asked to keep a :base of the whole buffer; then each edit is an :edit,
  where the text changed and how, and each change to the insets (see
  hoatzin.app.insets) an :insets of all of them, at most every
  `insets-delay-ms`, since typing in one changes them at every keystroke.
  A buffer whose changes are saved, or that is closed, is a :discard: there
  is nothing to lose. When the edits kept pass `compact-chars`, a new :base
  takes their place, which keeps the log from growing without end.

  Each op is a map with the :op and the buffer's :id; the host answers nil
  once it has kept them, or why it could not, and then the buffers they
  were of get a new :base at the next event. The edits are found by
  comparing texts, not by watching the editing: whatever changed the text
  is kept, and an undo or a replace is one edit as any other. Texts share
  the lines an edit left alone, so the comparison costs the lines it
  touched.

  :base  {:op :base :id n :gen g :meta {:path :name :scratch? :dir :mode}
          :text s :insets [...] :caret n}
         :gen is the generation: edits kept belong to the base of the same
         one, and a log of any other is a stale one
  :edit  {:op :edit :id n :gen g :lo n :old-count n :new s :caret n}
         [lo, lo+old-count) of the text became `new`
  :insets {:op :insets :id n :gen g :insets [...]}
  :discard {:op :discard :id n}

  The state is the app's :journal: {:buffers {id state} :error e}."
  (:require [clojure.string :as str]
            [hoatzin.app.buffers :as buffers]
            [hoatzin.app.files :as files]
            [hoatzin.app.insets :as insets]
            [hoatzin.lib.text :as text]))

(def compact-chars
  "How many characters of edits are kept before a new base replaces them."
  1000000)

(def insets-delay-ms
  "The longest the insets' changes wait to be kept."
  500)

;; ---------------------------------------------------------------- comparing texts

(defn- common-prefix [^String a ^String b]
  (let [n (min (count a) (count b))]
    (loop [i 0]
      (if (and (< i n) (= (.charAt a i) (.charAt b i))) (recur (inc i)) i))))

(defn- common-suffix
  "How many characters `a` and `b` share at their ends, not counting the
  first `skip` of either."
  [^String a ^String b skip]
  (let [la (count a), lb (count b), n (- (min la lb) skip)]
    (loop [i 0]
      (if (and (< i n) (= (.charAt a (- la 1 i)) (.charAt b (- lb 1 i)))) (recur (inc i)) i))))

(defn change
  "How text `b` differs from `a`, an earlier version of it, as one
  replacement: {:lo :old-count :new}, or nil if they are the same."
  [a b]
  (when-not (identical? a b)
    (let [[i ja jb] (text/changed-lines a b)
          ;; Whole lines, some on both sides: the newlines an edit added or
          ;; removed are then in what is compared.
          [i ja jb] (if (or (= i ja) (= i jb))
                      (if (pos? i) [(dec i) ja jb] [i (inc ja) (inc jb)])
                      [i ja jb])
          old (str/join "\n" (text/lines a i ja))
          new (str/join "\n" (text/lines b i jb))
          p   (common-prefix old new)
          s   (common-suffix old new p)
          n   (- (count old) p s)
          new (subs new p (- (count new) s))]
      (when (or (pos? n) (seq new))
        {:lo (+ (text/line-start a i) p) :old-count n :new new}))))

;; ---------------------------------------------------------------- observing

(defn- meta-of [b]
  {:path (:path b) :name (:buffer-name b) :scratch? (boolean (:scratch? b))
   :dir (:dir b) :mode (:major-mode b)})

(defn- rebase
  "[state ops] for keeping the whole of buffer `b`, anew."
  [now st b]
  (let [gen (inc (:gen st 0))
        ins (:insets b)
        doc (:doc b)]
    [{:text (:text doc) :insets ins :meta (meta-of b) :logged? true :gen gen
      :chars 0 :insets-sent ins :insets-at now}
     [{:op :base :id (:buffer-id b) :gen gen :meta (meta-of b)
       :text (str (:text doc)) :insets (insets/snapshot b) :caret (:caret doc)}]]))

(defn- step
  "[state ops] for buffer `b` as of `now`, given its `state` from before.
  With `flush?` the insets' changes are not left to wait."
  [now flush? st b]
  (let [id   (:buffer-id b)
        doc  (:doc b)
        text (:text doc)
        ins  (:insets b)]
    (cond
      ;; nothing to lose
      (not (:modified? b))
      [{:text text :insets ins :meta (meta-of b) :gen (:gen st 0) :logged? false}
       (when (:logged? st) [{:op :discard :id id}])]

      (or (not (:logged? st)) (not= (meta-of b) (:meta st)))
      (rebase now st b)

      :else
      (let [ch   (when-not (identical? text (:text st)) (change (:text st) text))
            size (+ 64 (count (:new ch)))]
        (if (> (+ (:chars st) size) compact-chars)
          (rebase now st b)
          (let [insets? (and (not (identical? ins (:insets-sent st)))
                             (or flush? (>= now (+ (:insets-at st) insets-delay-ms))))]
            ;; :insets is what the buffer has; :insets-sent, what was kept
            [(cond-> (assoc st :text text :insets ins)
               ch      (update :chars + size)
               insets? (assoc :insets-sent ins :insets-at now))
             (cond-> []
               ch      (conj {:op :edit :id id :gen (:gen st) :lo (:lo ch) :old-count (:old-count ch)
                              :new (:new ch) :caret (+ (:lo ch) (count (:new ch)))})
               insets? (conj {:op :insets :id id :gen (:gen st) :insets (insets/snapshot b)}))]))))))

(defn observe
  "The app, once what is new in its buffers is handed to :journal-fn: see
  the ns doc. Call it after every event, at time `now`."
  [app now]
  (let [app     (files/sync-modified app)
        current (select-keys app buffers/buffer-keys)
        bs      (cons current (remove #(= (:buffer-id %) (:buffer-id app)) (:buffers app)))
        states  (get-in app [:journal :buffers] {})
        flush?  (boolean (:quit? app))
        [states' ops] (reduce (fn [[sts ops] b]
                                (let [[st o] (step now flush? (get states (:buffer-id b)) b)]
                                  [(assoc sts (:buffer-id b) st) (into ops o)]))
                              [{} []]
                              bs)
        gone    (remove (set (map :buffer-id bs)) (keys states))
        ops     (into ops (keep #(when (:logged? (states %)) {:op :discard :id %}) gone))]
    (if (empty? ops)
      ;; the same app, when nothing changed: events that change nothing leave it alone
      (if (= states' states)
        app
        (assoc-in app [:journal :buffers] states'))
      (let [error (try ((:journal-fn app) ops)
                       (catch Exception e (or (ex-message e) (str e))))
            ;; what could not be kept is kept anew at the next event
            states' (if error
                      (reduce #(cond-> %1 (contains? %1 (:id %2)) (assoc-in [(:id %2) :logged?] false))
                              states' ops)
                      states')
            old-error (get-in app [:journal :error])]
        (cond-> (assoc app :journal {:buffers states' :error error})
          (and error (not= error old-error))
          (assoc :message (str "Can't keep recovery files: " error) :dirty? true))))))

(defn due-at
  "When the insets' changes are to be kept, if they are waiting to be, as a
  time in ms."
  [app]
  (some->> (get-in app [:journal :buffers])
           vals
           (keep #(when (and (:logged? %) (not (identical? (:insets %) (:insets-sent %))))
                    (+ (:insets-at %) insets-delay-ms)))
           seq
           (reduce min)))

(defn unsaved?
  "Whether a buffer has changes that the editor would not let go in a quit
  (the scratch buffer's are its own, see hoatzin.app.buffers/unsaved?):
  what the host keeps what is logged for, when it ends."
  [app]
  (boolean (some buffers/unsaved? (buffers/listing app))))
