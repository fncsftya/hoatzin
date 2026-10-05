(ns hoatzin.app.face
  "Faces: a font with its metrics and a cache of texts set as lines in it.
  The UI font (the status bar, the command line and boxes) is one, at
  the app's :ui."
  (:require [hoatzin.lib.coretext :as ct]
            [hoatzin.lib.layout :as layout]))

(def ui-lines-kept
  "How many texts in the UI font are kept set as lines: more than are on
  screen."
  512)

(defn face
  "A font of `family` at `size` points, its metrics, and a cache of up to
  `kept` texts set as lines in it."
  [app family size kept]
  (let [font (ct/font family (* size (:density app)))]
    {:font font :metrics (layout/metrics font) :lines (atom {}) :kept kept}))

(defn- release-face-lines! [{:keys [lines]}]
  (run! #(some-> (:line %) ct/release) (vals @lines))
  (reset! lines {}))

(defn release-face! [f]
  (release-face-lines! f)
  (ct/release-font (:font f)))

(defn release-family-faces!
  "Release the faces in `cache`, as `family-face` keeps them."
  [cache]
  (run! release-face! (vals @cache))
  (reset! cache {}))

(defn face-line
  "`text` set as a line in face `f`, as hoatzin.lib.coretext's line
  functions take it, with its UTF-16 :length; :line is nil for \"\". Once
  the face has `kept` texts set, they are all dropped: the command line's
  text changes with every keystroke."
  [f text]
  (let [lines (:lines f)]
    (or (get @lines text)
        (let [ln {:line (ct/make-line (:font f) text) :base 0
                  :length (ct/utf16-length text)}]
          (when (>= (count @lines) (:kept f)) (release-face-lines! f))
          (swap! lines assoc text ln)
          ln))))

(defn face-width
  "How wide `text` is in face `f`, in render pixels."
  [f text]
  (let [{:keys [line length] :as ln} (face-line f text)]
    (if line (long (Math/ceil (ct/offset-for-index ln length))) 0)))

(defn ui-width
  "How wide `text` is in the UI font, in render pixels."
  [app text]
  (face-width (:ui app) text))

(def ^:private family-faces-kept
  "How many faces `family-face` keeps: more than a dropdown's list shows."
  48)

(defn family-face
  "A face of font `family` at the UI font's size, to show its name in,
  kept in the app's :option-faces. Once that holds `family-faces-kept`,
  they are all dropped."
  [app family]
  (let [cache (:option-faces app)
        size  (get-in app [:fonts :ui-font :size])
        k     [family size (:density app)]]
    (or (get @cache k)
        (let [f (face app family size 4)]
          (when (>= (count @cache) family-faces-kept) (release-family-faces! cache))
          (swap! cache assoc k f)
          f))))

(defn ui-context
  "What hoatzin.lib.ui places boxes with: text one line high in the UI font."
  [app]
  {:scale (:density app)
   :text-size (fn [s] [(ui-width app s) (get-in app [:ui :metrics :line-height])])})
