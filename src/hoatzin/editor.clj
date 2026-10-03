(ns hoatzin.editor
  "The document being edited: its text and a caret, as code-point indices.
  Pure; where characters begin and end is the layout's business.")

(def empty-doc {:text "" :caret 0})

(defn insert
  "Insert `s` at the caret and move the caret past it."
  [{:keys [text caret] :as doc} s]
  (assoc doc
         :text  (str (subs text 0 caret) s (subs text caret))
         :caret (+ caret (count s))))

(defn delete
  "Delete the range between `a` and `b`; the caret lands where it was."
  [{:keys [text] :as doc} a b]
  (let [lo (min a b), hi (max a b)]
    (assoc doc :text (str (subs text 0 lo) (subs text hi)) :caret lo)))

(defn move [doc pos] (assoc doc :caret pos))
