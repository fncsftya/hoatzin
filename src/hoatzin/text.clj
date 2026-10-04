(ns hoatzin.text
  "The document's text: a persistent rope of lines.

  Text is a hoatzin.tree whose items are the lines, without their newlines.
  An edit copies the lines it touches and O(log n) tree nodes; every other
  line stays the same string object, shared with the text it came from. So
  the layout can tell which lines an edit changed by identity, and a large
  document costs no more to edit than a small one.

  Positions are code-point indices, as Jolt strings count them, with a
  newline between lines. `count` and `str` work on a Text, and two Texts
  are `=` when their contents are."
  (:refer-clojure :exclude [replace])
  (:require [clojure.string :as str]
            [hoatzin.tree :as tree]))

;; A line's :len is its length plus its newline, so a Text's length is its
;; root's :len less one: the last line has no newline.
(def ^:private spec {:len #(inc (count %)) :w (constantly 0)})

(declare lines)

(deftype Text [root]
  Object
  (equals [this o]
    (or (identical? this o)
        (and (instance? Text o)
             (= (tree/len root) (tree/len (.-root ^Text o)))
             (= (tree/n root) (tree/n (.-root ^Text o)))
             (= (lines this) (lines o)))))
  (hashCode [this] (hash (lines this)))
  (toString [this]
    (let [sb (StringBuilder.)]
      (tree/fold spec root 0 (tree/n root)
                 (fn [_ s k _ _] (when (pos? k) (.append sb "\n")) (.append sb s) nil)
                 nil)
      (.toString sb)))
  clojure.lang.Counted
  (count [this] (dec (tree/len root))))

(defn text? [x] (instance? Text x))

(defn- split-lines
  "`s` cut at its newlines; n newlines make n+1 lines."
  [s]
  (loop [from 0, acc (transient [])]
    (if-let [i (str/index-of s "\n" from)]
      (recur (inc i) (conj! acc (subs s from i)))
      (persistent! (conj! acc (subs s from))))))

(defn of
  "The Text of string `s` (or `s` itself, if it is a Text already)."
  [s]
  (if (text? s) s (Text. (tree/build spec (split-lines s)))))

(def empty-text (of ""))

(defn root [^Text t] (.-root t))

;; ---------------------------------------------------------------- lines

(defn line-count [t] (tree/n (root t)))

(defn line
  "Line `i`, without its newline."
  [t i]
  (tree/get-item spec (root t) i))

(defn lines
  "Lines [i, j) as a vector of strings."
  ([t] (lines t 0 (line-count t)))
  ([t i j] (tree/items spec (root t) i j)))

(defn line-start
  "The position where line `i` starts."
  [t i]
  (nth (tree/locate spec (root t) :n i) 2))

(defn line-at
  "The line holding position `pos`, as [index start line-text]. A position
  at a line's end, before its newline, belongs to that line."
  [t pos]
  (let [[s i start] (tree/locate spec (root t) :len pos)]
    [i start s]))

;; ---------------------------------------------------------------- characters

(defn char-at
  "The character at position `i`: a newline at the end of a line."
  [t i]
  (let [[_ start s] (line-at t i)
        k (- i start)]
    (if (< k (count s)) (nth s k) \newline)))

(defn slice
  "The text between positions `lo` and `hi`, as a string."
  [t lo hi]
  (let [[i a sa] (line-at t lo)
        [j b sb] (line-at t hi)]
    (if (= i j)
      (subs sa (- lo a) (- hi a))
      (let [out (StringBuilder.)]
        (.append out (subs sa (- lo a)))
        (tree/fold spec (root t) (inc i) j
                   (fn [_ s _ _ _] (.append out "\n") (.append out s) nil)
                   nil)
        (.append out "\n")
        (.append out (subs sb 0 (- hi b)))
        (.toString out)))))

(defn replace
  "The text with positions [lo, hi) replaced by string `s`."
  [t lo hi s]
  (let [[i a sa] (line-at t lo)
        [j b sb] (line-at t hi)
        joined (str (subs sa 0 (- lo a)) s (subs sb (- hi b)))]
    (Text. (tree/splice spec (root t) i (inc j)
                        (if (str/includes? s "\n") (split-lines joined) [joined])))))

(defn insert [t pos s] (replace t pos pos s))

;; ---------------------------------------------------------------- comparing

(defn changed-lines
  "Where `b` differs from `a`, an earlier version of it, by line: [i ja jb]
  says lines [i, ja) of `a` became lines [i, jb) of `b`, and the lines
  either side are the same strings in both. Cheap when they share history."
  [a b]
  (let [ra (root a), rb (root b)
        na (tree/n ra), nb (tree/n rb)
        p (tree/common-prefix spec ra rb)
        s (min (tree/common-suffix spec ra rb) (- (min na nb) p))]
    [p (- na s) (- nb s)]))
