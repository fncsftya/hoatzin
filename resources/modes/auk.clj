(ns hoatzin.modes.auk
  "Auk mode: notes, after Org, kept as EDN.

  An auk file is a map whose :content is a vector of the note's parts, in
  order, and whose :sections are the sections those parts refer to:

    {:content  [\"A line of the note.\"
                {:type :section :ref 1}
                {:type :list :content [{:text \"An item\"} {:text \"Another\"}]}
                {:type :checklist :content [{:text \"Done\" :checked? true}
                                            {:text \"To do\" :checked? false}]}
                {:type :hr}]
     :sections [{:id 1 :title \"Named\"
                 :content [\"A section: text of its own, in a box.\"
                           {:type :section :ref 2}]}
                {:id 2 :content [\"A section in a section.\"]}]}

  Each string is a line of the text. Each section is an inset (see
  hoatzin.app.insets) below the line before it, or above the first: a box
  holding text of its own, under a header that names it, by its :title if
  it has one, and folds and unfolds it. A section's :content is as the
  file's is, so sections hold sections; each is referred to once. A list
  or checklist is an inset too, its items each a line of its text, shown
  with a bullet or a box, ticked or not; and so is a horizontal rule, a
  line across the text, as HTML's <hr>.

  In normal mode:
    cmd+s       a section below the line the caret is in, in the text it
                is in, with a new line after it, and the caret in it
    cmd+shift+k delete the section, list or rule the caret is in or over,
                once asked
                (cmd+k deletes the line, as the editor does)
    space       fold the section the caret is in, leaving the caret over
                it; over a folded one, unfold it and go in
    cmd+r       rename the section the caret is in, or over
    l, ctrl+l   a list, or a checklist, as cmd+s makes a section; in a
                list, a sublist below the item the caret is in
    h           a horizontal rule, as cmd+s makes a section, the caret
                going to the line after it
    t           tick, or untick, the checklist item the caret is in
    cmd+shift+o a new line above the section or list the caret is in,
                or over, or before, in the text that holds it, as shift+o
                makes one above the line the caret is in; nothing is
                different on a line that is in neither
    k           delete forwards, as the editor does, but not a section
    e           export the note, as markdown or html, to a file beside
                this one: choose the format by its number
    tab         in a list, indent the item, then take it out to the list
                holding its list, then put it back where it was
  and in insert mode, cmd+ctrl+s makes a section, cmd+l and cmd+ctrl+l a list and a checklist,
  tab is as in normal mode, return on an empty last item of a list leaves
  it for a new line after it, and in either mode shift+return leaves every
  list the caret is in, or else the section, for a new line after it, in
  the text that holds it, and goes on in insert mode. Backspace at the start
  of a line below a section or list moves the line up into it. On an empty line, a section or list takes its place, the
  line after it. A click on a checklist's box ticks it, or unticks it.

  A file with anything else in it is refused rather than read, so that
  saving it could not lose what the editor doesn't yet understand."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [hoatzin.mode :as mode]))

;; ---------------------------------------------------------------- reading

(defn- refuse [why] (throw (ex-info why {})))

(defn- section-ref? [x]
  (and (map? x) (= #{:type :ref} (set (keys x))) (= :section (:type x))))

(defn- rule? [x] (= {:type :hr} x))

(declare list-part?)

(defn- item?
  "Whether `x` is an item of a list (or with `checklist?`, a checklist):
  a map with a :text and, in a checklist, perhaps :checked?."
  [checklist? x]
  (and (map? x) (string? (:text x))
       (every? (if checklist? #{:text :checked?} #{:text}) (keys x))
       (boolean? (:checked? x false))))

(defn- list-part?
  "Whether `x` is a list or checklist: its :content its items, and the
  lists below them, each after the item it is below."
  [x]
  (and (map? x) (= #{:type :content} (set (keys x)))
       (#{:list :checklist} (:type x))
       (vector? (:content x))
       (every? #(or (item? (= :checklist (:type x)) %) (list-part? %)) (:content x))))

(defn- check-content
  "`content`, a :content vector, as `where` has it; else refuse it."
  [where content]
  (when-not (and (vector? content) (every? #(or (string? %) (section-ref? %) (list-part? %) (rule? %)) content))
    (refuse (str where " must be a vector of strings, {:type :section :ref id},"
                 " {:type :list :content [...]}, {:type :checklist :content [...]}"
                 " and {:type :hr}")))
  content)

(defn- read-sections
  "The :sections of an auk file, by :id."
  [sections]
  (when-not (and (vector? sections) (every? map? sections))
    (refuse ":sections must be a vector of maps"))
  (reduce (fn [by {:keys [id title content] :as s}]
            (cond (not (#{#{:id :content} #{:id :title :content}} (set (keys s))))
                  (refuse "a section must have just :id, :content and perhaps :title")
                  (and (contains? s :title) (not (string? title)))
                  (refuse (str "section " (pr-str id) "'s :title must be a string"))
                  (contains? by id) (refuse (str "section " (pr-str id) " is in :sections twice"))
                  :else (assoc by id (assoc s :content (check-content (str "section " (pr-str id) "'s :content")
                                                                      content)))))
          {} sections))

(defn- list-doc
  "List or checklist `part` as an inset of a doc: its items the lines of
  its text, and its lists insets below the item before each."
  [part]
  (let [{:keys [items insets]}
        (reduce (fn [acc x]
                  (if (list-part? x)
                    (update acc :insets conj (assoc (list-doc x) :after (dec (count (:items acc)))))
                    (update acc :items conj x)))
                {:items [] :insets []}
                (:content part))
        items (if (seq items) items [{:text ""}])]
    (cond-> {:kind (:type part) :text (str/join "\n" (map :text items)) :insets insets}
      (= :checklist (:type part)) (assoc :checked (mapv #(boolean (:checked? %)) items)))))

(defn- doc-of
  "The doc (see hoatzin.app.modes) of :content vector `content`: its
  strings the lines of the text, and the sections it refers to, from
  `sections`, and its lists, insets below the line before each, the
  sections docs of their own :content. `seen` notes each section read,
  which may be read only once."
  [content sections seen]
  (let [{:keys [lines insets]}
        (reduce (fn [acc part]
                  (let [after (dec (count (:lines acc)))]
                    (cond
                      (string? part) (update acc :lines conj part)
                      (list-part? part) (update acc :insets conj (assoc (list-doc part) :after after))
                      (rule? part) (update acc :insets conj {:kind :rule :text "" :insets [] :after after})
                      :else
                      (let [id (:ref part)
                            {:keys [title] :as section} (sections id)]
                        (when-not section
                          (refuse (str "no section " (pr-str id))))
                        (when (contains? @seen id)
                          (refuse (str "section " (pr-str id) " is referred to twice")))
                        (vswap! seen conj id)
                        (update acc :insets conj
                                (cond-> (assoc (doc-of (:content section) sections seen) :after after)
                                  title (assoc :title title)))))))
                {:lines [] :insets []}
                content)]
    {:text (str/join "\n" lines) :insets insets}))

(defn- read-auk
  "The text and sections of auk file `s`, as a doc (see
  hoatzin.app.modes)."
  [s]
  (let [data (edn/read-string s)]
    (when-not (map? data)
      (refuse "not an auk file: expected a map"))
    (when-let [extra (seq (remove #{:content :sections} (keys data)))]
      (refuse (str "unsupported keys " (str/join ", " extra))))
    (let [sections (read-sections (:sections data []))
          seen     (volatile! #{})
          doc      (doc-of (check-content ":content" (:content data [])) sections seen)]
      (when-let [unused (seq (remove @seen (keys sections)))]
        (refuse (str "section " (pr-str (first unused)) " is not referred to")))
      doc)))

;; ---------------------------------------------------------------- writing

(defn- list-part
  "List or checklist `inset` as a part of :content, on one line: its
  items, and its lists after the items they are below."
  [{:keys [kind text checked insets]}]
  (let [items (str/split text #"\n" -1)
        below (group-by :after insets)
        item  (fn [k s]
                (str "{:text " (pr-str s)
                     (when (= :checklist kind) (str " :checked? " (boolean (get checked k))))
                     "}"))]
    (str "{:type " kind " :content ["
         (str/join " " (concat (map list-part (below -1))
                               (mapcat (fn [k s] (cons (item k s) (map list-part (below k))))
                                       (range) items)))
         "]}")))

(defn- write-auk
  "Doc `doc` as an auk file: a line of the text to each string of
  :content, its lists where they are, and the sections where they are,
  numbered as they come, each one's own :content the same."
  [doc]
  (let [sections (volatile! [])
        parts-of (fn parts-of [{:keys [text insets]}]
                   (let [lines (str/split text #"\n" -1)
                         ;; an empty text with everything above it is no line at all
                         lines (if (and (= [""] lines) (seq insets) (every? #(neg? (:after %)) insets))
                                 []
                                 lines)
                         below (group-by :after insets)
                         ;; the section's number before those of the sections in it
                         ref   (fn [inset]
                                 (let [id (inc (count @sections))
                                       _ (vswap! sections conj nil)
                                       content (parts-of inset)]
                                   (vswap! sections assoc (dec id)
                                           (cond-> {:id id :content content}
                                             (:title inset) (assoc :title (:title inset))))
                                   (str "{:type :section :ref " id "}")))]
                     (reduce (fn [parts item]
                               (conj parts (cond (string? item) (pr-str item)
                                                 (#{:list :checklist} (:kind item)) (list-part item)
                                                 (= :rule (:kind item)) "{:type :hr}"
                                                 :else (ref item))))
                             []
                             (concat (below -1) (mapcat (fn [i line] (cons line (below i))) (range) lines)))))
        parts (parts-of doc)]
    (str "{:content\n ["
         (str/join "\n  " parts)
         "]"
         (when (seq @sections)
           (str "\n :sections\n ["
                (str/join "\n  " (map (fn [{:keys [id title content]}]
                                        (str "{:id " id
                                             (when title (str " :title " (pr-str title)))
                                             " :content [" (str/join " " content) "]}"))
                                      @sections))
                "]"))
         "}\n")))

;; ---------------------------------------------------------------- exporting

(defn- blocks
  "The parts of `doc`, in order: a vector of lines for each paragraph (a
  run of lines that aren't blank), and each inset as it is."
  [{:keys [text insets]}]
  (let [below (group-by :after insets)
        parts (concat (below -1)
                      (mapcat (fn [i line] (cons line (below i)))
                              (range) (str/split text #"\n" -1)))]
    (:out
     (reduce (fn [{:keys [out open?] :as acc} part]
               (cond (not (string? part)) {:out (conj out part) :open? false}
                     (str/blank? part)    (assoc acc :open? false)
                     open?                {:out (conj (pop out) (conj (peek out) (str/trim part))) :open? true}
                     :else                {:out (conj out [(str/trim part)]) :open? true}))
             {:out [] :open? false}
             parts))))

(defn- items
  "The parts of list or checklist `inset`, in order: [:item text checked?]
  for each item, and the inset of each list below one."
  [{:keys [kind text checked insets]}]
  (let [below (group-by :after insets)]
    (concat (map (fn [i] i) (below -1))
            (mapcat (fn [k s] (cons [:item s (boolean (get checked k))] (below k)))
                    (range) (str/split text #"\n" -1)))))

(defn- md-list
  "The lines of list or checklist `inset` in markdown, `level` lists deep."
  [inset level]
  (mapcat (fn [x]
            (if (vector? x)
              [(str (apply str (repeat (* 2 level) " "))
                    (if (= :checklist (:kind inset)) (str "- [" (if (nth x 2) "x" " ") "] ") "* ")
                    (second x))]
              (md-list x (inc level))))
          (items inset)))

(defn- md-lines
  "The lines of `doc` in markdown, a blank line between its blocks. Its
  lines start with `q`, or with `blank` if they are blank: those of a
  section are quoted. The sections below it are indented `nest` and one
  level more than that, for those below them."
  [doc q blank nest]
  (let [piece (fn [block]
                (cond
                  (vector? block) (map-indexed (fn [i l] (str q l (when (< i (dec (count block))) "  "))) block)
                  :else
                  (case (:kind block)
                    :rule [(str q "---")]
                    (:list :checklist) (map #(str q %) (md-list block 0))
                    (let [n (str nest "> ")]
                               (md-lines (cond-> block
                                           (:title block) (update :text #(str "**" (:title block) "**\n\n" %)))
                                         n (str nest ">") (str nest "    "))))))]
    (->> (blocks doc)
         (map (comp vec piece))
         (interpose [blank])
         (apply concat))))

(defn- export-markdown [app]
  (str (str/join "\n" (md-lines (mode/doc app) "" "" "")) "\n"))

(defn- escape-html [s]
  (-> s (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")))

(defn- html-list
  "The lines of list or checklist `inset` in HTML."
  [inset]
  (let [checklist? (= :checklist (:kind inset))
        xs (vec (items inset))
        ;; the lists below an item go in its <li>
        groups (reduce (fn [gs x] (if (vector? x) (conj gs {:item x :lists []})
                                      (if (seq gs)
                                        (update gs (dec (count gs)) update :lists conj x)
                                        (conj gs {:item nil :lists [x]}))))
                       [] xs)]
    (concat ["<ul>"]
            (map #(str "  " %)
            (mapcat (fn [{:keys [item lists]}]
                      (let [open (str "<li>"
                                      (when item
                                        (str (when checklist?
                                               (str "<input type=\"checkbox\""
                                                    (when (nth item 2) " checked")
                                                    " onclick=\"return false;\"> "))
                                             (escape-html (second item)))))
                            inner (mapcat (fn [l] (map #(str "  " %) (html-list l))) lists)]
                        (if (seq inner)
                          (concat [open] inner ["</li>"])
                          [(str open "</li>")])))
                    groups))
            ["</ul>"])))

(defn- html-lines
  "The lines of `doc` in HTML; its sections below `depth` sections deep."
  [doc depth]
  (mapcat (fn [block]
            (cond
              (vector? block) [(str "<p>" (str/join "<br>\n" (map escape-html block)) "</p>")]
              :else
              (case (:kind block)
                :rule ["<hr>"]
                (:list :checklist) (html-list block)
                (concat ["<section>"]
                                 (when (:title block)
                                   [(str "  <h" (min 6 (inc depth)) ">" (escape-html (:title block))
                                         "</h" (min 6 (inc depth)) ">")])
                                 (map #(str "  " %) (html-lines block (inc depth)))
                                 ["</section>"]))))
          (blocks doc)))

(defn- export-html [app]
  (str "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n"
       "  <meta charset=\"utf-8\">\n"
       "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
       "  <title>" (escape-html (mode/file-title app)) "</title>\n</head>\n<body>\n"
       (str/join "\n" (map #(str "  " %) (html-lines (mode/doc app) 1)))
       "\n</body>\n</html>\n"))

(def ^:private exporters
  {"markdown" ["md" export-markdown]
   "html"     ["html" export-html]})

(defn- export
  "Ask for a format, then write the note beside its file in it."
  [app _]
  (mode/choose app ["markdown" "html"]
               (fn [app _ format]
                 (let [[ext f] (exporters format)]
                   (mode/write-beside app ext (f app))))))

;; ---------------------------------------------------------------- commands

(defn- section-path
  "The path of the innermost section the caret is in, or over, or nil."
  [app]
  (when-let [p (mode/current-inset app)]
    (some (fn [n] (let [q (subvec p 0 n)] (when (= :section (:kind (mode/inset-info app q))) q)))
          (range (count p) 0 -1))))

(defn- delete-inset
  "Ask to delete the section or list the caret is in, if it is in one."
  [app _]
  (if-let [p (mode/current-inset app)]
    (let [what (case (:kind (mode/inset-info app p))
                 :section "section" :list "list" :rule "rule" "checklist")]
      (mode/confirm app (str "Delete this " what "? (y/n)")
                    (fn [app now] (mode/message (mode/remove-inset app now p) (str "Deleted the " what)))))
    (mode/message app "The caret is not in a section or list, or over a rule")))

(defn- fold
  "Fold the section the caret is in, or unfold the one it is over."
  [app now]
  (if-let [p (section-path app)]
    (mode/toggle-inset app now p)
    app))

(defn- rename
  "Rename the section the caret is in, or over."
  [app _]
  (if-let [p (section-path app)]
    (mode/rename-inset app p)
    (mode/message app "The caret is not in a section")))

(defn- tick
  "Tick, or untick, the checklist item the caret is in."
  [app now]
  (let [p (mode/current-inset app)]
    (if (and p (= :checklist (:kind (mode/inset-info app p))))
      (mode/toggle-check app now p)
      (mode/message app "The caret is not in a checklist"))))

(defn- add [spec] (fn [app now] (mode/add-inset app now spec)))

{:name        "auk"
 :extensions  ["auk"]
 :read        read-auk
 :write       write-auk
 :inset-title "Section"
 :normal      {"cmd+s"  (add {})
               "e"      export
               "cmd+shift+k" delete-inset
               "cmd+shift+o" (fn [app now] (mode/line-above app now))
               " "      fold
               "cmd+r"  rename
               "l"      (add {:kind :list})
               "h"      (add {:kind :rule})
               "ctrl+l" (add {:kind :checklist})
               "t"      tick
               "tab"    (fn [app now] (mode/cycle-indent app now))
               "shift+return" (fn [app now] (mode/leave-inset app now))}
 :insert      {"cmd+ctrl+s"   (add {})
               "cmd+l"        (add {:kind :list})
               "cmd+ctrl+l"   (add {:kind :checklist})
               "tab"          (fn [app now] (mode/cycle-indent app now))
               "return"       (fn [app now] (mode/list-return app now false))
               "shift+return" (fn [app now] (mode/leave-inset app now))}
 :help        [["Auk mode"
                [["cmd+s" "add a section below the line (cmd+ctrl+s inserting)"]
                 ["cmd+shift+k" "delete section, list or rule"]
                 ["cmd+shift+o" "new line above the section or list"]
                 ["space" "fold or unfold the section"]
                 ["cmd+r" "rename the section"]
                 ["l" "add a list (cmd+l inserting)"]
                 ["ctrl+l" "add a checklist (cmd+ctrl+l inserting)"]
                 ["h" "add a rule below the line"]
                 ["t" "tick or untick the checklist item"]
                 ["e" "export as markdown or html"]
                 ["tab" "indent, outdent, restore a list item"]
                 ["return (twice)" "leave the list"]
                 ["shift+return" "new line after the lists, or the section"]
                 ["up / down" "into, out of, over and before sections"]
                 ["click a header" "fold or unfold a section"]]]]}
