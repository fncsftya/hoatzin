(ns hoatzin.app.input.keyboard
  "Keys and typing in the text. In :normal mode the text is left alone:
  keys move the caret and select, and typed text is a command (`i`, `:`).
  In :insert mode typing edits the text, as does the input method's
  composition, and escape goes back to :normal."
  (:require [hoatzin.app.command :as command]
            [hoatzin.app.geometry :refer [caret-or view-height]]
            [hoatzin.app.history :as history]
            [hoatzin.app.input.motion :refer [move-to move-on-line move-lines]]
            [hoatzin.app.insets :as insets]
            [hoatzin.app.state :refer [insert? touched enter-mode]]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.layout :as layout]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.text :as text]))

(defn- change-of
  "The [lo hi s] that `f` (ed/delete or ed/insert) does to `doc` with `args`."
  [doc f args]
  (if (= f ed/delete)
    [(apply min args) (apply max args) ""]
    (let [[lo hi] (or (ed/selection doc) [(:caret doc) (:caret doc)])]
      [lo hi (first args)])))

(defn- edit
  "Apply `f` (ed/delete or ed/insert) to the document, recording it for
  undo; normal mode leaves it alone."
  [app now f & args]
  (if (insert? app)
    (let [old (:doc app)
          [lo hi s] (change-of old f args)]
      (-> app
          (assoc :doc (apply f old args) :goal-x nil :upstream? false)
          (history/record old lo hi s)
          (touched now)))
    app))

(defn- normal? [app] (= :normal (:mode app)))

(defn- logical-line
  "The [start end] positions of the line (up to its newline) holding `pos`."
  [text pos]
  (let [[_ start s] (text/line-at (text/of text) pos)]
    [start (+ start (count s))]))

(defn- open-line
  "Start a new line below (or above) the caret's and insert there."
  [app now above?]
  (let [{:keys [text caret]} (:doc app)
        [start end] (logical-line text caret)
        pos (if above? start end)
        old (ed/move (:doc app) pos)
        doc (cond-> (ed/insert old "\n") above? (ed/move start))]
    (-> app (assoc :doc doc :goal-x nil :upstream? false)
        (history/record old pos pos "\n")
        (touched now) (enter-mode now :insert))))

(defn- select-with
  "Select the range `f` finds around the caret. With a selection, extend
  it to the end of the range `f` finds at the next non-whitespace after it."
  [app now f]
  (let [{:keys [text] :as doc} (:doc app)
        n   (count text)
        sel (ed/selection doc)
        at  (if sel (ed/skip-space text (second sel)) (:caret doc))
        [lo hi] (when (or (not sel) (< at n)) (f text (min at (max 0 (dec n)))))
        [lo hi] (if sel [(first sel) hi] [lo hi])]
    (if (and hi (< lo hi) (or (not sel) (> hi (second sel))))
      (-> app (assoc :doc (ed/select (ed/move doc lo) hi) :goal-x nil :upstream? false)
          (touched now))
      app)))

(defn- delete-forward
  "Delete the selection, or else the character after the caret, in normal
  mode as in insert mode; not the end of a paragraph an inset (see
  hoatzin.app.insets) is below, which would take the paragraph after it
  up past the inset."
  [app now]
  (let [{:keys [caret] :as doc} (:doc app)
        [lo hi] (or (ed/selection doc) [caret (layout/next-position (:layout app) caret)])]
    (if (or (= lo hi) (and (not (ed/selection doc)) (insets/inset-after? app caret)))
      app
      (-> app
          (assoc :doc (ed/delete doc lo hi) :goal-x nil :upstream? false)
          (history/record doc lo hi "")
          (touched now)))))

(defn- characters
  "`n` characters, as a message says it."
  [n]
  (str n (if (= 1 n) " character" " characters")))

(defn- copy!
  "Copy the selection to the clipboard, and say so."
  [app]
  (if-let [s (ed/selected-text (:doc app))]
    (do ((:set-clipboard-fn app) s)
        (assoc app :message (str "Copied " (characters (count s))) :dirty? true))
    app))

(defn- cut!
  "Cut the selection to the clipboard, and say so."
  [app now]
  (if-let [[lo hi] (ed/selection (:doc app))]
    (-> (copy! app)
        (assoc :doc (ed/delete (:doc app) lo hi) :goal-x nil :upstream? false
               :message (str "Cut " (characters (- hi lo))))
        (history/record (:doc app) lo hi "")
        (touched now))
    app))

(defn- paste!
  "Paste the clipboard at the caret, and say so."
  [app now]
  (let [s (text/normalize-newlines ((:clipboard-fn app)))]
    (if (seq s)
      (let [[lo hi] (or (ed/selection (:doc app)) [(get-in app [:doc :caret]) (get-in app [:doc :caret])])]
        (-> app (assoc :doc (ed/insert (:doc app) s) :goal-x nil :upstream? false
                       :message (str "Pasted " (characters (count s))))
            (history/record (:doc app) lo hi s)
            (touched now)))
      app)))

(defn on-key [app now key mod]
  (let [L      (:layout app)
        {:keys [text caret] :as doc} (:doc app)
        sel    (ed/selection doc)
        end    (count text)
        cmd?   (pos? (bit-and mod sdl/KMOD-GUI))
        shift? (pos? (bit-and mod sdl/KMOD-SHIFT))
        ctrl?  (pos? (bit-and mod sdl/KMOD-CTRL))
        ;; Shift moves the caret and drags the selection along. Otherwise a
        ;; selection collapses: keys going back start from its start, keys
        ;; going forward from its end.
        [back fwd] (if (and sel (not shift?)) sel [caret caret])
        line-of #(second (caret-or app %))
        go   #(move-to app now shift? %)
        to-line-start #(go (layout/line-start L (line-of %)))
        to-line-end #(let [k (line-of %)]
                       (move-on-line app now shift? k (layout/line-end L k)))
        page (max 1 (quot (view-height app) (layout/line-height L)))]
    (condp = key
      sdl/K-ESCAPE    (cond-> app
                        sel          (-> (assoc :doc (ed/move doc caret)) (touched now))
                        (insert? app) (enter-mode now :normal))
      sdl/K-BACKSPACE (cond sel        (edit app now ed/delete (first sel) (second sel))
                            cmd?       (edit app now ed/delete (layout/line-start L (line-of caret)) caret)
                            (pos? caret) (edit app now ed/delete (layout/prev-position L caret) caret)
                            :else      app)
      sdl/K-DELETE    (if sel
                        (edit app now ed/delete (first sel) (second sel))
                        (edit app now ed/delete caret (layout/next-position L caret)))
      sdl/K-RETURN    (edit app now ed/insert "\n")
      sdl/K-KP-ENTER  (edit app now ed/insert "\n")
      sdl/K-LEFT      (cond cmd?                   (go (ed/prev-word text back))
                            (and sel (not shift?)) (go back)
                            :else                  (go (layout/prev-position L caret)))
      sdl/K-RIGHT     (cond cmd?                   (go (ed/next-word text fwd))
                            (and sel (not shift?)) (go fwd)
                            :else                  (go (layout/next-position L caret)))
      sdl/K-HOME      (to-line-start back)
      sdl/K-END       (to-line-end fwd)
      sdl/K-UP        (if cmd? (go 0) (move-lines app now shift? back -1))
      sdl/K-DOWN      (if cmd? (go end) (move-lines app now shift? fwd 1))
      sdl/K-PAGEUP    (move-lines app now shift? back (- page))
      sdl/K-PAGEDOWN  (move-lines app now shift? fwd page)
      sdl/K-A         (cond cmd?  (-> app (assoc :doc (ed/select-all doc) :goal-x nil) (touched now))
                            ctrl? (if (insert? app) (to-line-start caret) app)
                            :else app)
      sdl/K-E         (if (and ctrl? (insert? app)) (to-line-end caret) app)
      sdl/K-C         (if cmd? (copy! app) app)
      sdl/K-K         (if (and cmd? (not shift?) (normal? app)) (insets/delete-line app now) app)
      sdl/K-X         (if (and cmd? (insert? app)) (cut! app now) app)
      sdl/K-V         (if (and cmd? (insert? app)) (paste! app now) app)
      app)))

(defn on-text
  "Typed text: inserted in insert mode, onto the command line in command
  mode, and a command in normal mode."
  [app now text]
  ;; Normal mode reads its commands from text, not keys: the `i` key is
  ;; followed by its text, which would otherwise be typed into the insert
  ;; mode it began.
  (case (:mode app)
    :insert  (edit (dissoc app :composition) now ed/insert text)
    :command (command/on-text app now text)
    (case text
      "i" (enter-mode app now :insert)
      "a" (let [{:keys [text caret] :as doc} (:doc app)
                pos (if-let [[_ hi] (ed/selection doc)]
                      hi
                      (if (< caret (count text)) (min (count text) (inc caret)) caret))
                pos (if (and (not (ed/selection doc)) (= \newline (text/char-at (text/of text) caret)))
                      caret
                      pos)]
            (-> (move-to app now false pos) (enter-mode now :insert)))
      "A" (-> (move-to app now false (second (logical-line (:text (:doc app)) (:caret (:doc app)))))
              (enter-mode now :insert))
      "o" (open-line app now false)
      "O" (open-line app now true)
      "c" (copy! app)
      "x" (cut! app now)
      "k" (delete-forward app now)
      "p" (paste! app now)
      "0" (move-to app now false (first (logical-line (:text (:doc app)) (:caret (:doc app)))))
      "^" (let [{:keys [text caret]} (:doc app)
                [start end] (logical-line text caret)
                t (text/of text)]
            (move-to app now false
                     (loop [j start]
                       (if (and (< j end) (Character/isWhitespace (text/char-at t j))) (recur (inc j)) j))))
      "w" (select-with app now ed/word-range)
      "s" (select-with app now ed/sentence-range)
      ":" (command/open-line app now)
      "g" (command/open-goto app now)
      "u" (history/undo app now)
      "r" (history/redo app now)
      "?" (assoc app :window :help :help-scroll 0 :dirty? true)
      app)))

(defn compose
  "Show (or, for \"\", end) the input method's composition at the caret.
  Composing replaces the selection, as typing does."
  [app now text cursor]
  (-> (if (empty? text)
        (dissoc app :composition)
        (let [n (count text)
              app (if-let [[lo hi] (ed/selection (:doc app))]
                    (-> app (assoc :doc (ed/delete (:doc app) lo hi))
                        (history/record (:doc app) lo hi ""))
                    app)]
          (assoc app :upstream? false :composition
                 {:text text :cursor (if (and cursor (<= 0 cursor n)) cursor n)})))
      (touched now)))
