(ns hoatzin.app.xsel
  "Selecting across texts. The text, the buffer's and each inset's (see
  hoatzin.app.insets), is one text to select in: the lines of a section or
  list are between the line it is below and the one after, and a
  selection from one place to another takes in whatever is between, in
  that order, wherever it is held.

  A selection is from an anchor to the head, where the caret is, each a
  key: the insets down to it, each as [after 1 order], its place among
  the lines and insets of the text holding it, and then, in the text the
  caret is in, [paragraph 0 position]; with nothing after the insets, the
  place of the caret over, or before, the last. Keys compare as the text
  reads. What is selected of each text, then, is the range between where
  the selection starts and ends in it, if there is any, and that is what
  each text's own selection is made, so that each shows it (see
  hoatzin.app.draw.text): :xsel, {:anchor key :a-state state :head key},
  says there is one. A selection that stays in one text is one too, if
  the buffer has insets, so as to take in those between; without, it is
  the text's own.

  A key that moves the caret and selects extends it, and cmd+a selects it
  all, and cmd+shift+a, all that is in the inset the caret is in; deleting
  what is selected leaves the insets that are not all of it, and anything
  else, that is not a way of selecting, gives it up before it is done."
  (:require [clojure.string :as str]
            [hoatzin.app.geometry :as geo]
            [hoatzin.app.history :as history]
            [hoatzin.app.insets :as insets]
            [hoatzin.app.state :refer [insert? touched]]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.layout :as layout]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.text :as text]))

;; ---------------------------------------------------------------- keys

(def ^:private neg-inf "Before everything in a text." [-2 0 0])
(def ^:private pos-inf "After everything in a text." [1000000000000 9 0])

(defn- ncmp [a b] (cond (< a b) -1 (> a b) 1 :else 0))

(defn- cmp-tuple [a b]
  (let [c (ncmp (a 0) (b 0))]
    (if-not (zero? c)
      c
      (let [c (ncmp (a 1) (b 1))]
        (if-not (zero? c) c (ncmp (a 2) (b 2)))))))

(defn- cmp-key
  "Whether key `a` is before (-1), the same as (0) or after (1) `b`; a
  key that begins another, before it."
  [a b]
  (let [na (count a), nb (count b)]
    (loop [i 0]
      (cond (and (= i na) (= i nb)) 0
            (= i na) -1
            (= i nb) 1
            :else (let [c (cmp-tuple (a i) (b i))]
                    (if (zero? c) (recur (inc i)) c))))))

(defn- slots
  "`level`'s insets' tuples, by id."
  [level]
  (into {} (map (fn [e] [(:id e) [(:after e) 1 (:order e)]])) (insets/ordered level)))

(defn- paragraph-of [level pos]
  (first (text/line-at (text/of (get-in level [:doc :text])) pos)))

(defn- level-at [app path]
  (reduce (fn [l id] (get-in l [:insets id])) app path))

(defn- key-of
  "The key of position `pos` in the text at `path`, or with no `pos` of
  the inset at its end, itself."
  [app path pos]
  (loop [level app, ids path, ks []]
    (if (seq ids)
      (recur (get-in level [:insets (first ids)]) (rest ids) (conj ks ((slots level) (first ids))))
      (if pos (conj ks [(paragraph-of level pos) 0 pos]) ks))))

(defn- state
  "Where the caret is: {:path :pos :kind}, :kind :text, or :over for
  where it is over, or before, the inset at the end of :path."
  [app]
  (let [path (vec (insets/path app))]
    (if (insets/over app)
      {:path path :pos nil :kind :over}
      {:path path :pos (get-in (level-at app path) [:doc :caret]) :kind :text})))

(defn- state-key [app {:keys [path pos]}] (key-of app path pos))

(defn- head-key [app] (state-key app (state app)))

(defn- anchor-state
  "Where a selection begins, as the caret is: if the text it is in has a
  selection of its own, from that selection's anchor."
  [app]
  (let [{:keys [path] :as st} (state app)
        anchor (when (= :text (:kind st)) (get-in (level-at app path) [:doc :anchor]))]
    (cond-> st anchor (assoc :pos anchor))))

;; ---------------------------------------------------------------- ranges

(defn- relation
  "Where key `b` is to the text of a level whose insets' tuples are `tuples`:
  [:before], [:after], [:at pos] or [:slot j], held by an inset of it
  below paragraph j (-1: above the first)."
  [b tuples]
  (let [m  (count tuples), nb (count b)
        c  (cmp-key (subvec b 0 (min m nb)) tuples)]
    (cond (neg? c)  [:before]
          (pos? c)  [:after]
          (= nb m)  [:before]
          :else     (let [[x y z] (b m)]
                      (case (long y) 0 [:at z], 1 [:slot x], [:after])))))

(defn- coverage
  "The range [lo hi] of the text of `level`, with insets' tuples `tuples`,
  that is selected from key `from` to key `to`, or nil."
  [level tuples from to]
  (let [t  (text/of (get-in level [:doc :text]))
        n  (count t)
        np (text/line-count t)
        a  (let [[r p] (relation from tuples)]
             (case r :before 0, :after nil, :at p, :slot (when (< (inc p) np) (text/line-start t (inc p)))))
        b  (let [[r p] (relation to tuples)]
             (case r :before nil, :after n, :at p,
                   :slot (when (>= p 0) (+ (text/line-start t p) (count (text/line t p))))))]
    (when (and a b (<= a b)) [a b])))

(defn- included?
  "Whether any of the text in the inset with key `ik` is between `from`
  and `to`."
  [ik from to]
  (and (pos? (cmp-key to ik)) (neg? (cmp-key from (conj ik pos-inf)))))

;; ---------------------------------------------------------------- showing it

(defn- spread
  "`level` with each text's own selection that of the selection from `from`
  to `to` there; in the text at `head` (a path) with the caret at the end
  `to-end?` is of."
  [level tuples path from to head to-end?]
  (let [ts  (slots level)
        cov (coverage level tuples from to)
        doc (:doc level)
        doc (if-let [[a b] cov]
              (if (and (= path head) (not to-end?))
                (-> doc (ed/move b) (ed/select a))
                (-> doc (ed/move a) (ed/select b)))
              (ed/move doc (:caret doc)))]
    (cond-> (assoc level :doc doc)
      (seq (:insets level))
      (update :insets (fn [is] (reduce-kv (fn [is id i]
                                            (assoc is id (spread i (conj tuples (ts id)) (conj path id)
                                                                 from to head to-end?)))
                                          {} is))))))

(defn- show
  "The app with the selection `xsel` shown in each text."
  [app {:keys [anchor head] :as xsel} head-path]
  (let [[from to] (if (neg? (cmp-key head anchor)) [head anchor] [anchor head])]
    (assoc (spread app [] [] from to head-path (not (neg? (cmp-key head anchor)))) :xsel xsel)))

(defn clear
  "The app with no selection across texts: each text's own is given up."
  [app]
  (if (:xsel app)
    (letfn [(collapse [level]
              (cond-> (update level :doc #(ed/move % (:caret %)))
                (seq (:insets level)) (update :insets update-vals collapse)))]
      (dissoc (collapse app) :xsel))
    app))

;; ---------------------------------------------------------------- extending

(defn- finish
  "The app, its caret moved as the selection from `anchor` (a key) and its
  :a-state `a-st` is extended to, with the selection shown."
  [app now anchor a-st]
  (let [st   (state app)
        head (state-key app st)]
    (-> (if (zero? (cmp-key anchor head))
          (clear app)
          (show app {:anchor anchor :a-state a-st :head head} (when (= :text (:kind st)) (:path st))))
        (touched now))))

(defn- motion-key? [key mod]
  (let [ctrl? (pos? (bit-and mod sdl/KMOD-CTRL))
        cmd?  (pos? (bit-and mod sdl/KMOD-GUI))]
    (boolean (or (#{sdl/K-LEFT sdl/K-RIGHT sdl/K-UP sdl/K-DOWN sdl/K-HOME sdl/K-END
                    sdl/K-PAGEUP sdl/K-PAGEDOWN} key)
                 (and ctrl? (not cmd?) (#{sdl/K-A sdl/K-E} key))
                 (and cmd? (= key sdl/K-M))))))

(def ^:private far "Pixels: past the ends of any line." 10000000)

(defn- crossing
  "[stop x] for `key`, shift or not, if it takes the caret out of the
  text it is in, or from over an inset: the next, or previous, stop (see
  hoatzin.app.insets/stop-from), at a line's start or end where left or
  right goes there; else nil."
  [app key]
  (let [over? (boolean (insets/over app))
        down? (or (= key sdl/K-DOWN) (and over? (= key sdl/K-RIGHT)))
        up?   (or (= key sdl/K-UP) (and over? (= key sdl/K-LEFT)))
        take  (fn [down?] (when-let [[[q k :as stop] x pre over-id] (insets/stop-from app down?)]
                            [stop x (or over-id (not= q pre) (not (number? k)))]))]
    (cond
      (or down? up?)
      (let [[stop x out?] (take down?)] (when out? [stop x]))

      (and (not over?) (#{sdl/K-LEFT sdl/K-RIGHT} key))
      (let [{:keys [caret text]} (:doc (insets/innermost-view app))
            [_ start line] (text/line-at (text/of text) caret)
            right? (= key sdl/K-RIGHT)]
        (when (= caret (if right? (+ start (count line)) start))
          (let [[[_ k :as stop] x out?] (take right?)]
            (when out? [stop (if (number? k) (if right? (- far) far) x)])))))))

(defn- extend-selection
  "Extend the selection with the caret-moving `key`, as `run` moves it."
  [app now key mod run]
  (let [anchor (or (get-in app [:xsel :anchor]) (state-key app (anchor-state app)))
        a-st   (or (get-in app [:xsel :a-state]) (anchor-state app))
        app'   (if-let [[stop x] (and (zero? (bit-and mod sdl/KMOD-GUI)) (crossing app key))]
                 (insets/go app now stop x)
                 (run app))]
    (finish app' now anchor a-st)))

;; ---------------------------------------------------------------- selecting all

(defn- go-end
  "The app with the caret at the end of the text at `path`, and what is in
  it, or nil."
  [app now path]
  (when-let [stop (insets/end-stop app path)]
    (insets/go app now stop far)))

(defn- select-in
  "The app with all of the text at `path`, and the insets in it, selected,
  the caret at the end; nil if it is folded."
  [app now path]
  (let [path (vec path)
        base (key-of app path nil)]
    (when-let [app' (go-end (clear app) now path)]
      (let [st (state app')]
        (-> (show app' {:anchor (conj base neg-inf) :a-state {:path path :pos 0 :kind :text}
                        :head (conj base pos-inf)}
                  (when (= :text (:kind st)) (:path st)))
            (assoc :goal-x nil)
            (touched now))))))

(defn- select-all [app now] (select-in app now []))

(defn- select-within
  "Select all that is in the inset the caret is in; with none, all."
  [app now]
  (if-let [p (insets/path app)]
    (or (select-in app now p) app)
    (select-all app now)))

;; ---------------------------------------------------------------- what is selected

(defn- lines-of
  "The paragraphs of `level`'s text, and of its insets', that are
  selected between `from` and `to`, in order: partly, or all."
  [level tuples from to]
  (let [t    (text/of (get-in level [:doc :text]))
        cov  (coverage level tuples from to)
        ts   (slots level)
        by   (group-by :after (insets/ordered level))
        blk  (fn [after]
               (mapcat (fn [e]
                         (let [ik (conj tuples (ts (:id e)))]
                           (when (included? ik from to)
                             (lines-of (get-in level [:insets (:id e)]) ik from to))))
                       (by after)))]
    (concat (blk -1)
            (mapcat (fn [j]
                      (concat (when cov
                                (let [s (text/line-start t j)
                                      e (+ s (count (text/line t j)))
                                      a (max s (first cov))
                                      b (min e (second cov))]
                                  (when (<= a b) [(str (text/slice t a b))])))
                              (blk j)))
                    (range (text/line-count t))))))

(defn- bounds [app]
  (let [{:keys [anchor head]} (:xsel app)]
    (if (neg? (cmp-key head anchor)) [head anchor] [anchor head])))

(defn selected-text
  "What is selected across the texts, as lines, or nil."
  [app]
  (when (:xsel app)
    (let [[from to] (bounds app)]
      (str/join "\n" (lines-of app [] from to)))))

(defn- characters [n] (str n (if (= 1 n) " character" " characters")))

(defn- copy!
  "Copy what is selected to the clipboard, and say so."
  [app]
  (let [s (selected-text app)]
    ((:set-clipboard-fn app) s)
    (assoc app :message (str "Copied " (characters (count s))) :dirty? true)))

;; ---------------------------------------------------------------- deleting

(defn- fully?
  "Whether all of the text of `level`, and of its insets, is selected."
  [level tuples from to]
  (let [ts  (slots level)
        cov (coverage level tuples from to)
        n   (count (text/of (get-in level [:doc :text])))]
    (and (= [0 n] cov)
         (every? (fn [[id i]]
                   (let [ik (conj tuples (ts id))]
                     (and (included? ik from to) (fully? i ik from to))))
                 (:insets level)))))

(defn- delete-in
  "`level` without what is selected of its text, and its insets' texts;
  the insets that are all of it go too."
  [level tuples from to]
  (let [ts    (slots level)
        level (reduce (fn [level [id i]]
                        (let [ik (conj tuples (ts id))]
                          (cond (not (included? ik from to)) level
                                (fully? i ik from to)
                                (-> level (update :doc ed/unmark (insets/mark-id id)) (update :insets dissoc id))
                                :else (assoc-in level [:insets id] (delete-in i ik from to)))))
                      level (:insets level))
        cov   (coverage level tuples from to)]
    (if (and cov (< (first cov) (second cov)))
      (let [[a b] cov
            old   (:doc level)]
        (-> level
            (assoc :doc (ed/delete old a b) :goal-x nil :upstream? false)
            (dissoc :undo-tail)
            (history/record old a b "")))
      level)))

(defn- landing
  "Where the caret goes once what is selected is gone: [path pos], the
  start of the selection, or if that is gone, the start of what was
  deleted in the text that held it."
  [app]
  (let [{:keys [anchor a-state head]} (:xsel app)
        [from to] (bounds app)
        st    (if (neg? (cmp-key head anchor)) (state app) a-state)
        path  (:path st)
        tup   (fn [d] (let [p (subvec path 0 d)] [p (key-of app p nil) (level-at app p)]))
        starts (mapv (fn [d] (let [[_ tuples level] (tup d)]
                               (or (first (coverage level tuples from to))
                                   (get-in level [:doc :caret]))))
                     (range (inc (count path))))]
    {:path path :pos (when (= :text (:kind st)) (:pos st)) :starts starts}))

(defn- land
  "The app with the caret where `landing` said, as far as that is still
  there."
  [app {:keys [path pos starts]}]
  (let [n (loop [level app, ids path, n 0]
            (if (and (seq ids) (get-in level [:insets (first ids)]))
              (recur (get-in level [:insets (first ids)]) (rest ids) (inc n))
              n))
        here (subvec path 0 n)
        len  (count (get-in (level-at app here) [:doc :text]))
        at   (if (and (= n (count path)) pos) pos (starts n))]
    (insets/caret-to app here (max 0 (min len at)))))

(defn delete-selection
  "The app without what is selected across texts, the caret where it
  began."
  [app now]
  (let [[from to] (bounds app)
        l   (landing app)
        app (clear app)
        app (merge (apply dissoc app insets/text-keys)
                   (select-keys (delete-in app [] from to) insets/text-keys))]
    (-> (land app l)
        (dissoc :selecting?)
        (assoc :goal-x nil :upstream? false)
        (touched now))))

;; ---------------------------------------------------------------- events

(defn- modifier-key?
  "Whether `key` is one of the modifiers, pressed on its own."
  [key]
  (<= 0x400000e0 key 0x400000e7))

(defn on-key
  "The app after key `key` with modifiers `mod` where it has to do with
  a selection across texts, or nil where it has not. `run` is what the key
  does otherwise, to an app."
  [app now key mod run]
  (when (or (:xsel app) (seq (:insets app)))
    (let [cmd?   (pos? (bit-and mod sdl/KMOD-GUI))
          shift? (pos? (bit-and mod sdl/KMOD-SHIFT))
          xsel?  (some? (:xsel app))
          select? (or shift? (:selecting? app))]
      (cond
        ;; a modifier pressed alone does nothing, and leaves the selection
        (modifier-key? key) app

        (and (= key sdl/K-A) cmd?)
        (if shift? (select-within app now) (select-all app now))

        (and select? (motion-key? key mod))
        (extend-selection app now key mod run)

        (not xsel?) nil

        ;; a character's key comes before its text, which says what to do
        (and (< 0x20 key 0x7f)
             (zero? (bit-and mod (bit-or sdl/KMOD-GUI sdl/KMOD-CTRL sdl/KMOD-ALT))))
        app

        (and cmd? (= key sdl/K-C)) (copy! app)
        (and cmd? (= key sdl/K-X) (insert? app)) (delete-selection (copy! app) now)
        (and (#{sdl/K-BACKSPACE sdl/K-DELETE} key) (insert? app)) (delete-selection app now)
        (and (#{sdl/K-RETURN sdl/K-KP-ENTER} key) (insert? app)) (run (delete-selection app now))
        (and cmd? (= key sdl/K-V) (insert? app)) (run (delete-selection app now))

        :else (run (clear app))))))

(defn on-text
  "The app after `text` typed where it has to do with a selection across
  texts, or nil where it has not. `run` is what it does otherwise, to an
  app."
  [app now text run]
  (when (:xsel app)
    (if (insert? app)
      (run (delete-selection app now))
      (case text
        "c" (clear (copy! app))
        "x" (delete-selection (copy! app) now)
        "k" (delete-selection app now)
        "p" (run (delete-selection app now))
        (run (clear app))))))

;; ---------------------------------------------------------------- the mouse

(defn- under
  "Where render pixel (x, y) is in `level`, at any depth: {:path :pos}, or
  with :over, over the inset at the end of :path."
  [level path x y]
  (if-let [{:keys [id part]} (insets/hit level x y)]
    (let [i (get-in level [:insets id])
          v (when (= :body part) (insets/view level id))]
      (cond v (under v (conj path id) x y)
            (:collapsed? i) {:path (conj path id) :over? true}
            :else {:path (conj path id) :pos 0}))
    (let [[k gx] (geo/point->line level x y)]
      {:path path :pos (layout/position-at (:layout level) k gx)})))

(defn drag
  "The app after the pointer, with the button down, moved to render pixel
  (x, y): the selection from where the click was to where the pointer is,
  across texts; nil if there is none to make, as when there are no insets,
  or it is by words."
  [app now x y]
  (when (and (:dragging? app) (not (:drag-word app)) (seq (:insets app)))
    (let [{a-st :a-state anchor :anchor} (or (:xdrag app)
                                             {:anchor (state-key app (anchor-state app))
                                              :a-state (anchor-state app)})
          {:keys [path pos over?]} (under app [] x y)
          app (assoc app :xdrag {:anchor anchor :a-state a-st} :drag-point [x y])
          app (if over?
                (insets/go app now [path :header] x)
                (insets/caret-to app path pos))]
      (finish app now anchor a-st))))
