(ns hoatzin.modes.auk
  "Auk mode: notes, after Org, kept as EDN.

  An auk file is a map whose :content is a vector of the note's parts, in
  order, and whose :sections are the sections those parts refer to:

    {:content  [\"A line of the note.\"
                {:type :section :ref 1}
                \"Another.\"]
     :sections [{:id 1 :content \"A section: text of its own, in a box.\"}]}

  Each string is a line of the text. Each section is an inset (see
  hoatzin.app.insets) below the line before it, or above the first: a box
  holding text of its own, folded and unfolded by its header. cmd+s adds
  one, below the line the caret is in, and cmd+k deletes the one the
  caret is in, once asked.

  A file with anything else in it is refused rather than read, so that
  saving it could not lose what the editor doesn't yet understand."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [hoatzin.mode :as mode]))

(defn- refuse [why] (throw (ex-info why {})))

(defn- section-ref? [x]
  (and (map? x) (= #{:type :ref} (set (keys x))) (= :section (:type x))))

(defn- read-sections
  "The :sections of an auk file, by :id."
  [sections]
  (when-not (and (vector? sections) (every? map? sections))
    (refuse ":sections must be a vector of maps"))
  (reduce (fn [by {:keys [id content] :as s}]
            (cond (not= #{:id :content} (set (keys s))) (refuse "a section must have just :id and :content")
                  (not (string? content)) (refuse (str "section " (pr-str id) "'s :content must be a string"))
                  (contains? by id) (refuse (str "section " (pr-str id) " is in :sections twice"))
                  :else (assoc by id content)))
          {} sections))

(defn- read-auk
  "The text and sections of auk file `s`, as a doc (see
  hoatzin.app.modes)."
  [s]
  (let [data (edn/read-string s)]
    (when-not (map? data)
      (refuse "not an auk file: expected a map"))
    (when-let [extra (seq (remove #{:content :sections} (keys data)))]
      (refuse (str "unsupported keys " (str/join ", " extra))))
    (let [content  (:content data [])
          sections (read-sections (:sections data []))]
      (when-not (and (vector? content) (every? #(or (string? %) (section-ref? %)) content))
        (refuse ":content must be a vector of strings and {:type :section :ref id}"))
      (let [refs (keep :ref content)]
        (when-let [missing (seq (remove sections refs))]
          (refuse (str "no section " (pr-str (first missing)))))
        (when-let [twice (seq (for [[id n] (frequencies refs) :when (> n 1)] id))]
          (refuse (str "section " (pr-str (first twice)) " is in :content twice")))
        (when-let [unused (seq (remove (set refs) (keys sections)))]
          (refuse (str "section " (pr-str (first unused)) " is not in :content"))))
      (let [{:keys [lines insets]}
            (reduce (fn [acc part]
                      (if (string? part)
                        (update acc :lines conj part)
                        (update acc :insets conj {:after (dec (count (:lines acc)))
                                                  :text (sections (:ref part))})))
                    {:lines [] :insets []}
                    content)]
        {:text (str/join "\n" lines) :insets insets}))))

(defn- write-auk
  "Doc `doc` as an auk file: a line of the text to each string of
  :content, and the sections, numbered down the text, where they are."
  [{:keys [text insets]}]
  (let [lines (str/split text #"\n" -1)
        ;; an empty text with every section above it is no line at all
        lines (if (and (= [""] lines) (seq insets) (every? #(neg? (:after %)) insets)) [] lines)
        numbered (map-indexed (fn [i inset] (assoc inset :id (inc i))) insets)
        below (group-by :after numbered)
        ref   (fn [{:keys [id]}] (str "{:type :section :ref " id "}"))
        parts (concat (map ref (below -1))
                      (mapcat (fn [i line] (cons (pr-str line) (map ref (below i))))
                              (range) lines))]
    (str "{:content\n ["
         (str/join "\n  " parts)
         "]"
         (when (seq insets)
           (str "\n :sections\n ["
                (str/join "\n  " (map (fn [{:keys [id text]}]
                                        (str "{:id " id " :content " (pr-str text) "}"))
                                      numbered))
                "]"))
         "}\n")))

(defn- delete-section
  "Ask to delete the section the caret is in, if it is in one."
  [app _]
  (if-let [id (mode/current-inset app)]
    (mode/confirm app "Delete this section? (y/n)"
                  (fn [app now] (mode/message (mode/remove-inset app now id) "Deleted the section")))
    (mode/message app "The caret is not in a section")))

{:name        "auk"
 :extensions  ["auk"]
 :read        read-auk
 :write       write-auk
 :inset-title "Section"
 :normal      {"cmd+s" (fn [app now] (mode/add-inset app now))
               "cmd+k" delete-section}
 :help        [["Auk mode"
                [["cmd+s" "add a section below the line"]
                 ["cmd+k" "delete the section (asks first)"]
                 ["up / down" "into and out of a section"]
                 ["click a header" "fold or unfold a section"]]]]}
