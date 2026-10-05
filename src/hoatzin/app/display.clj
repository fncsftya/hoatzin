(ns hoatzin.app.display
  "The text as shown: the document, plus any input-method composition at
  the caret. The layout is of this text, so its positions are the
  document's only while nothing is being composed."
  (:require [hoatzin.lib.text :as text]))

(defn display-text
  "The text as shown: the document, plus any composition at the caret."
  [{:keys [doc composition]}]
  (if composition
    (let [{:keys [text caret]} doc]
      (text/insert text caret (:text composition)))
    (:text doc)))

(defn display-key
  "What the display text is a function of: see `same-display?`."
  [{:keys [doc composition]}]
  (if composition [(:text doc) (:caret doc) (:text composition)] (:text doc)))

(defn same-display?
  "Whether display keys `a` and `b` show the same text. The document's text
  compares by identity: every edit makes a new one, and comparing contents
  could walk the whole document."
  [a b]
  (if (vector? a)
    (and (vector? b) (identical? (a 0) (b 0)) (= (subvec a 1) (subvec b 1)))
    (identical? a b)))

(defn view-caret
  "Where the caret is drawn: inside the composition while composing."
  [{:keys [doc composition]}]
  (+ (:caret doc) (if composition (:cursor composition) 0)))

(defn shown-pos
  "Where document position `pos` is in the display text: past the
  composition, if that is inserted before it."
  [{:keys [doc composition]} pos]
  (if (and composition (> pos (:caret doc))) (+ pos (count (:text composition))) pos))
