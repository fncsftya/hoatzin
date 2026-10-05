(ns hoatzin.app.history
  "Undo and redo, after Emacs. Every edit is recorded as a change, [lo, lo+n)
  of the text replaced by another string, and changes are kept in :undo, a
  vector of groups, the newest last. Typing and deleting a character at a
  time join the group before them, up to `group-limit` characters, until
  the caret moves or the mode changes.

  Undoing is an edit too: it applies the inverse of a group's changes and
  records them, as a group of their own (marked :undo?). A run of undos
  works back down the groups from where it began, `:undo-chain` keeping its
  place, and breaks as soon as anything else changes the text or the caret.
  Undoing again after that undoes the undos, as in Emacs. Redo is that: it
  starts a run of undos from the newest group, if that is an undo, and
  carries on through the undos before it."
  (:require [clojure.string :as str]
            [hoatzin.app.state :refer [touched]]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.text :as text]))

(def group-limit "The most characters typed or deleted in one group." 20)

(defn reset
  "Forget the history, as for a new file."
  [app]
  (dissoc app :undo :undo-tail :undo-chain))

(defn- groupable?
  "Whether change `c` carries on from `prev`, as typing or deleting does."
  [prev c]
  (let [typed? #(and (= "" (:old %)) (not (str/includes? (:new %) "\n")))
        cut?   #(and (= "" (:new %)) (seq (:old %)))]
    (or (and (typed? prev) (typed? c)
             (= (:lo c) (+ (:lo prev) (count (:new prev)))))
        (and (cut? prev) (cut? c)
             (or (= (+ (:lo c) (count (:old c))) (:lo prev))    ; backspace
                 (= (:lo c) (:lo prev)))))))                    ; delete

(defn record
  "The app with the change made to `old-doc`, which gave the app's :doc, in
  its history: [lo, hi) replaced by string `s`."
  [app old-doc lo hi s]
  (let [old (text/slice (:text old-doc) lo hi)]
    (if (and (= "" old) (= "" s))
      app
      (let [c     {:lo lo :old old :new s}
            tail  (:undo-tail app)
            undo  (:undo app [])
            last-group (peek undo)
            join? (and tail last-group (not (:undo? last-group))
                       (identical? (:text tail) (:text old-doc))
                       (= (:caret tail) (:caret old-doc))
                       (nil? (:anchor old-doc))
                       (= (:n tail) (:mode-count app 0))
                       (< (:chars tail) group-limit)
                       (groupable? (peek (:changes last-group)) c))
            undo  (if join?
                    (conj (pop undo) (update last-group :changes conj c))
                    (conj undo {:changes [c]}))]
        (assoc app :undo undo
               :undo-tail {:text (get-in app [:doc :text]) :caret (get-in app [:doc :caret])
                           :n (:mode-count app 0)
                           :chars (+ (if join? (:chars tail) 0) (max (count old) (count s)))})))))

(defn- apply-change
  "The document with change `c` made, and the change that undoes it."
  [doc {:keys [lo old new]}]
  (let [doc (cond-> (ed/delete doc lo (+ lo (count new))) (seq old) (-> (ed/move lo) (ed/insert old)))]
    [(ed/move doc (+ lo (count old))) {:lo lo :old new :new old}]))

(defn- undo-group
  "The app with group `g` undone, the undoing recorded as a group."
  [app g]
  (let [[doc inverse] (reduce (fn [[doc inv] c]
                                (let [[doc i] (apply-change doc c)] [doc (conj inv i)]))
                              [(:doc app) []]
                              (rseq (:changes g)))]
    (-> app
        (assoc :doc doc :goal-x nil :upstream? false)
        (update :undo (fnil conj []) {:changes inverse :undo? true})
        (dissoc :undo-tail))))

(defn- run
  "One step of an undo run of `kind` (:undo or :redo): continue the run in
  progress, or begin one at the newest group. A redo is only an undo of
  undos."
  [app now kind none-message]
  (let [chain  (:undo-chain app)
        {:keys [text caret anchor]} (:doc app)
        live?  (and chain (= kind (:kind chain))
                    (identical? text (:text chain)) (= caret (:caret chain)) (nil? anchor))
        at     (if live? (:at chain) (count (:undo app [])))
        g      (when (pos? at) (nth (:undo app) (dec at)))]
    (if (or (nil? g) (and (= kind :redo) (not (:undo? g))))
      (assoc app :message none-message :dirty? true)
      (let [app (undo-group app g)]
        (-> app
            (assoc :undo-chain {:kind kind :at (dec at)
                                :text (get-in app [:doc :text]) :caret (get-in app [:doc :caret])}
                   :message (if (= kind :undo) "Undo" "Redo"))
            (touched now))))))

(defn undo [app now] (run app now :undo "No further undo information"))
(defn redo [app now] (run app now :redo "Nothing to redo"))
