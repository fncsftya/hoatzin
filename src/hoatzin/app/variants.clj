(ns hoatzin.app.variants
  "Variants, a minor mode (see hoatzin.app.modes), on in every buffer
  unless `:minor variants` turns it off: other wordings of a part of the
  text, kept beside it, any one of which the text may show.

  In normal mode:
    v       add a variant of the selection, or of the variant the caret
            is over: the text goes, the caret, an underline, is where it
            was, and what is typed there is the variant; return keeps it
            and escape gives it up, putting the text back
    n       over a variant, show the next of its wordings in its place
  A variant shows as up to three dots below the start of its text, one
  for each of its wordings. Its text may be edited as any other: the
  wording it shows becomes what is there.

  Each variant is a pair of marks in the buffer's text (see
  hoatzin.lib.editor), at its start and end, and in :variants, by id,
  {:options [s ...] :selected i}: its wordings, and the one shown. They
  are of the buffer's own text, not its insets'.

  They are kept beside the file they are of, as EDN, written as the file
  is: {:variants [{:start i :end j :options [s ...] :selected k} ...]},
  :start and :end positions in the buffer's text, in
  modes/variants/<file> in the hoatzin config directory, <file> the
  file's absolute path with each / a -, and .edn after it. As a buffer
  visits its file, they are asked of the host, which reads them on a
  thread of its own and gives them back as a :mode-data event (see the
  hoatzin.app ns doc): one whose wording isn't what the text has there
  is left out."
  (:require [clojure.string :as str]
            [hoatzin.app.display :as display]
            [hoatzin.app.geometry :as geo]
            [hoatzin.app.history :as history]
            [hoatzin.app.state :refer [px touched enter-mode]]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.layout :as layout]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.text :as text]))

(def ^:private mode-name "variants")
(def ^:private most-dots "The most dots a variant shows." 3)

(defn- message [app s] (assoc app :message s :dirty? true))

;; ---------------------------------------------------------------- the marks

(defn- start-id [id] [:variant id :start])
(defn- end-id [id] [:variant id :end])

(defn- mark [doc id start end] (-> doc (ed/mark (start-id id) start) (ed/mark (end-id id) end)))
(defn- unmark [doc id] (-> doc (ed/unmark (start-id id)) (ed/unmark (end-id id))))

(defn- live
  "The variants of `level` (the app, or a buffer of it) that still have
  text, as [id [start end]], in order down it. Deleting a variant's text
  leaves its marks together, as nothing."
  [level]
  (let [marks (get-in level [:doc :marks])]
    (->> (keys (:variants level))
         (keep (fn [id]
                 (let [s (get marks (start-id id)), e (get marks (end-id id))]
                   (when (and s e (< s e)) [id [s e]]))))
         (sort-by (comp first second)))))

(defn- pruned
  "`level` without the variants whose text is gone."
  [level]
  (let [gone (remove (set (map first (live level))) (keys (:variants level)))]
    (-> level
        (update :doc #(reduce unmark % gone))
        (update :variants #(apply dissoc % gone)))))

(defn- over
  "The variant the caret is over, as [id [start end]], or nil. Only the
  buffer's own text has them."
  [app]
  (when-not (:inset app)
    (let [caret (get-in app [:doc :caret])]
      (some (fn [[_ [s e] :as v]] (when (and (<= s caret) (< caret e)) v)) (live app)))))

(defn- index-of [xs x] (first (keep-indexed #(when (= x %2) %1) xs)))

(defn- synced
  "Variant `v` as text `s` shows it: the wording `s` is, else with `s` in
  place of the wording it showed."
  [{:keys [options selected] :as v} s]
  (cond (= s (get options selected)) v
        (some #{s} options) (assoc v :selected (index-of options s))
        :else (assoc-in v [:options selected] s)))

(defn- with-synced
  "`level` with each variant as its text shows it."
  [level]
  (let [level (pruned level)
        t     (get-in level [:doc :text])]
    (reduce (fn [l [id [s e]]] (update-in l [:variants id] synced (text/slice t s e)))
            level (live level))))

;; ---------------------------------------------------------------- adding one

(defn- edit-end
  "The end of the variant being typed."
  [app]
  (let [{:keys [lo base]} (:variant-edit app)]
    (+ lo (- (count (get-in app [:doc :text])) base))))

(defn- start-adding
  "Begin adding a variant of the selection, or of the variant the caret
  is over: its text goes, and what is typed in its place is the variant."
  [app now]
  (if (:inset app)
    (message app "Variants are only of the buffer's own text")
    (let [app   (pruned app)
          doc   (:doc app)
          spans (live app)
          [lo hi] (or (ed/selection doc) (second (over app)))
          group (some (fn [[id span]] (when (= span [lo hi]) id)) spans)]
      (cond
        (nil? lo)
        (message app "Select the text to add a variant of")

        (and (not group) (some (fn [[_ [s e]]] (and (< s hi) (< lo e))) spans))
        (message app "The selection overlaps a variant")

        :else
        (let [doc' (-> doc (cond-> group (unmark group)) (ed/delete lo hi))]
          (-> app
              (assoc :doc doc' :goal-x nil :upstream? false :caret-shape :underline
                     :variant-edit {:lo lo :base (count (:text doc'))
                                    :original (text/slice (:text doc) lo hi) :group group
                                    :anchor (:anchor doc) :caret (:caret doc)})
              (history/record doc lo hi "")
              (touched now)
              (enter-mode now :insert)))))))

(defn- finish [app now]
  (-> app (dissoc :variant-edit :caret-shape) (touched now) (enter-mode now :normal)))

(defn- cancel
  "Give up the variant being typed, putting back the text it was of."
  [app now]
  (let [{:keys [lo original group anchor caret]} (:variant-edit app)
        end (edit-end app)
        old (:doc app)
        doc (-> old (ed/move lo) (ed/select end) (ed/insert original)
                (cond-> group (mark group lo (+ lo (count original)))))
        doc (if anchor (-> doc (ed/move anchor) (ed/select caret)) (ed/move doc caret))]
    (-> app
        (assoc :doc doc :goal-x nil :upstream? false)
        (history/record old lo end original)
        (finish now))))

(defn- accept
  "Keep the variant typed, a wording of the text it was typed over, which
  the text now shows; the caret goes to its start."
  [app now]
  (let [{:keys [lo original group]} (:variant-edit app)
        end (edit-end app)
        s   (text/slice (get-in app [:doc :text]) lo end)
        v   (if group
              (synced (get-in app [:variants group]) original)
              {:options [original] :selected 0})
        options (if (some #{s} (:options v)) (:options v) (conj (:options v) s))
        v   (assoc v :options options :selected (index-of options s))
        id  (or group (:next-variant-id app 0))
        n   (count options)]
    (cond
      (empty? s)  (message (cancel app now) "A variant can't be empty")
      (= 1 n)     (message (cancel app now) "The variant is the text as it was")
      :else
      (-> app
          (update :doc #(-> % (mark id lo end) (ed/move lo)))
          (assoc-in [:variants id] v)
          (assoc :next-variant-id (max (:next-variant-id app 0) (inc id)))
          (finish now)
          (message (str "Variant " (inc (:selected v)) " of " n))))))

(defn- on-event
  "While a variant is being typed: return keeps it, escape (or a click)
  gives it up, and the caret keeps to it. nil for the events it leaves to
  the text."
  [app now event]
  (when (:variant-edit app)
    (let [{:keys [type key mod] :or {mod 0}} event
          caret (get-in app [:doc :caret])
          selected? (some? (ed/selection (:doc app)))
          lo    (:lo (:variant-edit app))
          end   (edit-end app)
          bare? (zero? (bit-and mod (bit-or sdl/KMOD-GUI sdl/KMOD-CTRL sdl/KMOD-ALT)))
          cmd?  (= sdl/KMOD-GUI (bit-and mod (bit-or sdl/KMOD-GUI sdl/KMOD-CTRL sdl/KMOD-ALT)))]
      (case type
        :key   (cond
                 (:composition app)                    nil
                 (= key sdl/K-ESCAPE)                  (cancel app now)
                 (#{sdl/K-RETURN sdl/K-KP-ENTER} key)  (accept app now)
                 (and bare? (= key sdl/K-LEFT))       (when-not (> caret lo) app)
                 (and bare? (= key sdl/K-RIGHT))      (when-not (< caret end) app)
                 (and bare? (= key sdl/K-BACKSPACE))  (when-not (or (> caret lo) selected?) app)
                 (and bare? (= key sdl/K-DELETE))     (when-not (or (< caret end) selected?) app)
                 ;; copy, cut and paste: what is selected is in the variant
                 (and cmd? (#{sdl/K-C sdl/K-X sdl/K-V} key)) nil
                 :else app)
        :click (cancel app now)
        nil))))

;; ---------------------------------------------------------------- choosing one

(defn- next-variant
  "Show the next wording of the variant the caret is over, the caret at
  its start; nil if it is over none."
  [app now]
  (when-let [[id [s e]] (over app)]
    (let [doc (:doc app)
          v   (synced (get-in app [:variants id]) (text/slice (:text doc) s e))
          n   (count (:options v))
          i   (mod (inc (:selected v)) n)
          t   (get-in v [:options i])]
      (-> app
          (assoc :doc (-> doc (ed/move s) (ed/select e) (ed/insert t) (ed/move s))
                 :goal-x nil :upstream? false)
          (assoc-in [:variants id] (assoc v :selected i))
          (history/record doc s e t)
          (touched now)
          (message (str "Variant " (inc i) " of " n))))))

;; ---------------------------------------------------------------- the file

(defn file-for
  "The name of the file the variants of the file at `path` are kept in."
  [path]
  (str (str/replace path "/" "-") ".edn"))

(defn- opened
  "Ask the host for the variants of the buffer's file."
  [app]
  (when-let [path (:path app)]
    ((:load-mode-data-fn app) mode-name (file-for path)))
  app)

(defn- written
  "Keep the buffer's variants beside its file, as just written; with
  none, keep none."
  [app]
  (if-let [path (:path app)]
    (let [app   (with-synced app)
          data  (when-let [vs (seq (live app))]
                  {:variants (mapv (fn [[id [s e]]]
                                     (let [{:keys [options selected]} (get-in app [:variants id])]
                                       {:start s :end e :options options :selected selected}))
                                   vs)})
          error ((:save-mode-data-fn app) mode-name (file-for path) data)]
      (cond-> app error (message (str (:message app) "; can't write variants: " error))))
    app))

(defn- variant?
  "Whether `v`, as read, is a variant of text `t`, showing what `t` has
  there."
  [t {:keys [start end options selected] :as v}]
  (and (map? v) (integer? start) (integer? end) (<= 0 start) (< start end) (<= end (count t))
       (vector? options) (>= (count options) 2) (every? string? options)
       (= (count options) (count (set options)))
       (integer? selected) (< -1 selected (count options))
       (= (get options selected) (text/slice t start end))))

(defn- load-into
  "`level`, a buffer, with the variants of `data`, as read, unless it
  has its own already: each that isn't one of its text, or overlaps one
  before it, is left out."
  [level data]
  (let [level (pruned level)
        vs    (when (map? data) (:variants data))]
    (if (or (seq (:variants level)) (not (sequential? vs)))
      level
      (let [t (get-in level [:doc :text])]
        (reduce (fn [l {:keys [start end options selected] :as v}]
                  (if (and (variant? t v)
                           (not-any? (fn [[_ [s e]]] (and (< s end) (< start e))) (live l)))
                    (let [id (:next-variant-id l 0)]
                      (-> l
                          (update :doc mark id start end)
                          (assoc-in [:variants id] {:options options :selected selected})
                          (assoc :next-variant-id (inc id))))
                    l))
                level vs)))))

(defn- loaded
  "The variants the host read, as a :mode-data event has them, given to
  the buffer visiting the file they are of, if one does; or why they
  couldn't be read. The variant being typed, if any, keeps the buffer's
  as they are."
  [app {:keys [file data error]}]
  (let [for? #(= file (some-> (:path %) file-for))]
    (cond
      (and (for? app) error)
      (message app (str "Can't read variants: " error))

      error app

      (for? app)
      (if (:variant-edit app) app (assoc (load-into app data) :dirty? true))

      :else
      (update app :buffers
              (fn [bs] (mapv #(if (and (for? %) (not= (:buffer-id %) (:buffer-id app))) (load-into % data) %)
                             bs))))))

;; ---------------------------------------------------------------- drawing

(defn- draw!
  "Below the start of each variant on visual lines [k0, k1), a dot for
  each of its wordings, up to `most-dots`."
  [app k0 k1]
  (when-not (:composition app)
    (let [{:keys [renderer layout scroll scratch]} app
          {:keys [baseline]} (:metrics layout)
          [ox oy] (geo/origin app)
          side  (max 1 (px app 2))
          gap   (max 1 (px app 2))
          below (max 1 (px app 3))
          [r g b] (:ui-accent app)]
      (sdl/set-render-draw-color renderer r g b 255)
      (doseq [[id [s _]] (live app)
              :let [[x k] (layout/caret layout (display/shown-pos app s))]
              :when (and (<= k0 k) (< k k1))]
        (let [x (+ ox (long (Math/floor x)))
              y (+ oy (- (geo/line-top app k) scroll) baseline below)]
          (dotimes [i (min most-dots (count (get-in app [:variants id :options])))]
            (sdl/render-fill-rect renderer
                                  (sdl/set-frect! (:frect scratch) (+ x (* i (+ side gap))) y side side))))))))

(def mode
  {:name     mode-name
   :minor?   true
   :default? true
   :normal   {"v" start-adding
              "n" next-variant}
   :opened   opened
   :written  written
   :loaded   loaded
   :on-event on-event
   :status   (fn [app] (when (:variant-edit app) "Variant: return keeps, esc cancels"))
   :draw     draw!
   :help     [["Variants"
               [["v" "add a variant of the selection, or the variant"]
                ["return / esc" "keep the variant typed / give it up"]
                ["n" "show the variant's next wording"]]]]})
