(ns hoatzin.app.input.keyboard
  "Keys and typing in the text. In :normal mode the text is left alone:
  keys move the caret and select, and typed text is a command (`i`, `:`).
  In :insert mode typing edits the text, as does the input method's
  composition, and escape goes back to :normal."
  (:require [hoatzin.app.command :as command]
            [hoatzin.app.geometry :refer [caret-or view-height]]
            [hoatzin.app.input.motion :refer [move-to move-on-line move-lines]]
            [hoatzin.app.state :refer [insert? touched enter-mode]]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.layout :as layout]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.text :as text]))

(defn- edit
  "Apply `f` to the document; normal mode leaves it alone."
  [app now f & args]
  (if (insert? app)
    (-> app (assoc :doc (apply f (:doc app) args) :goal-x nil :upstream? false) (touched now))
    app))

(defn- copy! [app]
  (when-let [s (ed/selected-text (:doc app))]
    ((:set-clipboard-fn app) s)))

(defn on-key [app now key mod]
  (let [L      (:layout app)
        {:keys [text caret] :as doc} (:doc app)
        sel    (ed/selection doc)
        end    (count text)
        cmd?   (pos? (bit-and mod sdl/KMOD-GUI))
        shift? (pos? (bit-and mod sdl/KMOD-SHIFT))
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
      sdl/K-ESCAPE    (if (insert? app) (enter-mode app now :normal) app)
      sdl/K-BACKSPACE (cond sel        (edit app now ed/delete (first sel) (second sel))
                            cmd?       (edit app now ed/delete (layout/line-start L (line-of caret)) caret)
                            (pos? caret) (edit app now ed/delete (layout/prev-position L caret) caret)
                            :else      app)
      sdl/K-DELETE    (if sel
                        (edit app now ed/delete (first sel) (second sel))
                        (edit app now ed/delete caret (layout/next-position L caret)))
      sdl/K-RETURN    (edit app now ed/insert "\n")
      sdl/K-KP-ENTER  (edit app now ed/insert "\n")
      sdl/K-LEFT      (cond cmd?                   (to-line-start back)
                            (and sel (not shift?)) (go back)
                            :else                  (go (layout/prev-position L caret)))
      sdl/K-RIGHT     (cond cmd?                   (to-line-end fwd)
                            (and sel (not shift?)) (go fwd)
                            :else                  (go (layout/next-position L caret)))
      sdl/K-HOME      (to-line-start back)
      sdl/K-END       (to-line-end fwd)
      sdl/K-UP        (if cmd? (go 0) (move-lines app now shift? back -1))
      sdl/K-DOWN      (if cmd? (go end) (move-lines app now shift? fwd 1))
      sdl/K-PAGEUP    (move-lines app now shift? back (- page))
      sdl/K-PAGEDOWN  (move-lines app now shift? fwd page)
      sdl/K-A         (if cmd? (-> app (assoc :doc (ed/select-all doc) :goal-x nil) (touched now)) app)
      sdl/K-C         (do (when cmd? (copy! app)) app)
      sdl/K-X         (if (and cmd? sel (insert? app))
                        (do (copy! app) (edit app now ed/delete (first sel) (second sel)))
                        app)
      sdl/K-V         (if cmd?
                        (let [s (text/normalize-newlines ((:clipboard-fn app)))]
                          (if (seq s) (edit app now ed/insert s) app))
                        app)
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
      ":" (command/open-line app now)
      app)))

(defn compose
  "Show (or, for \"\", end) the input method's composition at the caret.
  Composing replaces the selection, as typing does."
  [app now text cursor]
  (-> (if (empty? text)
        (dissoc app :composition)
        (let [n (count text)
              app (if-let [[lo hi] (ed/selection (:doc app))]
                    (assoc app :doc (ed/delete (:doc app) lo hi))
                    app)]
          (assoc app :upstream? false :composition
                 {:text text :cursor (if (and cursor (<= 0 cursor n)) cursor n)})))
      (touched now)))
