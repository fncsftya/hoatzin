(ns hoatzin.editor
  "The document being edited: its text, a caret and, while something is
  selected, an :anchor where the selection began. Positions are code-point
  indices. Pure; where characters begin and end is the layout's business.

  The selection runs between the anchor and the caret, in either order. There
  is no :anchor when nothing is selected, so an anchor never equals the caret.

  :marks, when there are any, maps ids to positions that move with the text
  as it is edited: what is inserted before a mark pushes it along, and a
  mark inside text that is deleted lands where the deletion was.

  The text is a hoatzin.text rope, so edits cost the same however long the
  document is."
  (:require [hoatzin.text :as text]))

(def empty-doc {:text text/empty-text :caret 0})

(defn doc
  "A document of string `s`, with the caret at `caret` (or the start)."
  ([s] (doc s 0))
  ([s caret] {:text (text/of s) :caret caret}))

(defn selection
  "The selected range as [lo hi], or nil when nothing is selected."
  [{:keys [anchor caret]}]
  (when anchor [(min anchor caret) (max anchor caret)]))

(defn selected-text [{:keys [text] :as doc}]
  (when-let [[lo hi] (selection doc)] (text/slice text lo hi)))

(defn mark
  "Mark position `pos` as `id`, replacing any mark `id` was."
  [doc id pos]
  (assoc-in doc [:marks id] pos))

(defn unmark [doc id]
  (let [marks (dissoc (:marks doc) id)]
    (if (empty? marks) (dissoc doc :marks) (assoc doc :marks marks))))

(defn- move-marks
  "The marks after [lo, hi) was replaced by `n` characters. A mark at `lo`
  stays put: text typed at a mark goes after it."
  [doc lo hi n]
  (if-let [marks (:marks doc)]
    (assoc doc :marks (update-vals marks #(cond (<= % lo) %
                                                (>= % hi) (+ % (- n (- hi lo)))
                                                :else     lo)))
    doc))

(defn delete
  "Delete the range between `a` and `b`; the caret lands where it was."
  [{:keys [text] :as doc} a b]
  (let [lo (min a b), hi (max a b)]
    (-> doc
        (assoc :text (text/replace text lo hi "") :caret lo)
        (dissoc :anchor)
        (move-marks lo hi 0))))

(defn insert
  "Insert `s` at the caret, replacing any selection, and move the caret past it."
  [{:keys [text caret] :as doc} s]
  (let [[lo hi] (or (selection doc) [caret caret])]
    (-> doc
        (assoc :text (text/replace text lo hi s) :caret (+ lo (count s)))
        (dissoc :anchor)
        (move-marks lo hi (count s)))))

(defn move
  "Move the caret to `pos`, dropping any selection."
  [doc pos]
  (-> doc (assoc :caret pos) (dissoc :anchor)))

(defn select
  "Move the caret to `pos`, extending the selection from where it began
  (or from the caret, if nothing was selected)."
  [{:keys [anchor caret] :as doc} pos]
  (let [anchor (or anchor caret)]
    (if (= anchor pos)
      (move doc pos)
      (assoc doc :anchor anchor :caret pos))))

(defn- char-class [c]
  (cond (= c \newline)            :newline
        (Character/isWhitespace c) :space
        :else                      :word))

(defn word-range
  "The [lo hi] run around the character at index `i` of `text` (a string or
  a hoatzin.text): non-whitespace (a word), or whitespace if that is what
  is there. Newlines end both, and a run of newlines is a run of its own."
  [text i]
  (let [t (text/of text)
        [_ start s] (text/line-at t i)
        k (- i start)]
    (if (= k (count s))
      ;; a newline: the run crosses the empty lines either side
      (let [nl? #(= \newline (text/char-at t %))
            n (count t)]
        [(loop [j i] (if (and (pos? j) (nl? (dec j))) (recur (dec j)) j))
         (loop [j (inc i)] (if (and (< j n) (nl? j)) (recur (inc j)) j))])
      ;; otherwise the run stays within the line
      (let [cls   (char-class (nth s k))
            same? #(= cls (char-class (nth s %)))]
        [(+ start (loop [j k] (if (and (pos? j) (same? (dec j))) (recur (dec j)) j)))
         (+ start (loop [j (inc k)] (if (and (< j (count s)) (same? j)) (recur (inc j)) j)))]))))

(defn select-word
  "Select the word (or run of whitespace) around the character at `i`."
  [{:keys [text] :as doc} i]
  (let [[lo hi] (word-range text i)]
    (select (move doc lo) hi)))

(defn select-all [{:keys [text] :as doc}]
  (select (move doc 0) (count text)))
