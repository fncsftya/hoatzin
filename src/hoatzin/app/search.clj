(ns hoatzin.app.search
  "Searching the buffer's text, a minor mode (see hoatzin.app.modes), off
  unless a search turns it on.

  In normal mode `/` opens a prompt in the status bar: what is typed is
  the query, return accepts it and escape gives it up. Accepted, the
  caret jumps to the next match at or after it, or the status bar says
  there are none, and the buffer is in search mode, where
    n       jumps to the next match, wrapping to the first
    N       (shift+n) jumps to the previous match, wrapping to the last
    /       opens the prompt again, with the query to change
    escape  leaves search mode
  Each match shows behind a yellow tint. A query with no capital letters
  matches any case; one with them matches as typed. The keys of search
  mode come before those of the modes turned on before it, variants' `n`
  among them. Only the buffer's own text is
  searched, not its insets'."
  (:require [clojure.string :as str]
            [hoatzin.app.display :as display]
            [hoatzin.app.geometry :as geo]
            [hoatzin.app.insets :as insets]
            [hoatzin.app.modes :as modes]
            [hoatzin.app.state :refer [touched enter-mode]]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.layout :as layout]
            [hoatzin.lib.sdl :as sdl]))

(def ^:private mode-name "search")
(def ^:private highlight "The colour of a match." [255 217 0])
(def ^:private highlight-alpha "How opaque a match's tint is, of 255." 110)

(defn- message [app s] (assoc app :message s :dirty? true))

;; ---------------------------------------------------------------- matching

(defn matches
  "The matches of `query` in `text`, as [start end], in order down it
  and apart; none for an empty query."
  [text query]
  (if (str/blank? (str query))
    []
    (let [s      (str text)
          fold?  (= query (str/lower-case query))
          hay    (if fold? (str/lower-case s) s)
          n      (count query)]
      (loop [from 0, acc (transient [])]
        (let [i (.indexOf ^String hay ^String query (int from))]
          (if (neg? i)
            (persistent! acc)
            (recur (+ i n) (conj! acc [i (+ i n)]))))))))

(def ^:private cache (atom nil))

(defn- found
  "The matches of the app's query in its text, as `matches`, kept while
  neither changes."
  [app]
  (let [text  (get-in app [:doc :text])
        query (:search app)
        [t q ms] @cache]
    (if (and (identical? t text) (= q query))
      ms
      (let [ms (matches text query)]
        (reset! cache [text query ms])
        ms))))

(defn- jump
  "The app with the caret at the start of match [s _], in view."
  [app now [s]]
  (-> (insets/leave app now)
      (assoc :doc (ed/move (:doc app) s) :goal-x nil :upstream? false)
      (touched now)))

(defn- caret [app] (get-in app [:doc :caret]))

(defn- step
  "The app jumped to the first match after the caret (`forward?`) or the
  last before it, wrapping round the text; says so if there is none."
  [app now forward?]
  (let [ms (found app)
        c  (caret app)
        m  (if forward?
             (or (first (filter #(> (first %) c) ms)) (first ms))
             (or (last (filter #(< (first %) c) ms)) (last ms)))]
    (if m
      (jump app now m)
      (message app (str "No results: " (:search app))))))

;; ---------------------------------------------------------------- the prompt

(defn open
  "Open the search prompt in the status bar, empty, or, in search mode,
  holding the query, to change."
  [app now]
  (-> app (enter-mode now :command)
      (assoc :command (if (contains? (:minor-modes app) mode-name) (or (:search app) "") "")
             :search? true)))

(defn prompt-text
  "The prompt as shown."
  [app]
  (str "/" (:command app)))

(defn- leave [app now]
  (-> app (enter-mode now :normal) (dissoc :command :search?)))

(defn- accept
  "Search for what was typed: into search mode, at the first match at or
  after the caret."
  [app now]
  (let [query (:command app)
        app   (leave app now)]
    (if (str/blank? query)
      app
      (let [app (assoc app :search query)
            app (if (contains? (:minor-modes app) mode-name) app (modes/toggle-minor app mode-name))
            c   (caret app)
            m   (or (first (filter #(>= (first %) c) (found app))) (first (found app)))]
        (if m
          (-> (jump app now m) (dissoc :message))
          (message app (str "No results: " query)))))))

(defn on-text
  "Typed text, onto the end of the query."
  [app now text]
  (-> app (update :command str text) (assoc :dirty? true :blink-from now)))

(defn on-key
  "A key at the prompt."
  [app now key]
  (let [q (:command app)]
    (condp = key
      sdl/K-ESCAPE   (leave app now)
      sdl/K-RETURN   (accept app now)
      sdl/K-KP-ENTER (accept app now)
      ;; backspacing past the `/` leaves the prompt
      sdl/K-BACKSPACE (if (empty? q)
                        (leave app now)
                        (assoc app :command (subs q 0 (dec (count q))) :dirty? true :blink-from now))
      app)))

;; ---------------------------------------------------------------- the mode

(defn- end-search [app now]
  (-> (modes/toggle-minor app mode-name) (dissoc :search :message) (touched now)))

(defn- draw-under!
  "A tint behind each match on visual lines [k0, k1)."
  [app k0 k1]
  (let [{:keys [renderer layout scroll scratch]} app
        lh (layout/line-height layout)
        [ox oy] (geo/origin app)
        [r g b] highlight]
    (sdl/set-render-draw-blend-mode renderer sdl/BLENDMODE-BLEND)
    (sdl/set-render-draw-color renderer r g b highlight-alpha)
    (doseq [[s e] (found app)
            [k x0 x1] (layout/range-segments layout (display/shown-pos app s) (display/shown-pos app e) k0 k1)
            :let [x0 (long (Math/floor x0))]]
      (sdl/render-fill-rect renderer
                            (sdl/set-frect! (:frect scratch) (+ ox x0) (+ oy (- (geo/line-top app k) scroll))
                                            (- (long (Math/ceil x1)) x0) lh)))
    (sdl/set-render-draw-blend-mode renderer sdl/BLENDMODE-NONE)))

(def mode
  {:name       mode-name
   :minor?     true
   :normal     {"n"      (fn [app now] (step app now true))
                "N"      (fn [app now] (step app now false))
                "/"      open
                "escape" end-search}
   :status     (fn [app]
                 (when-not (:message app)
                   (str "Search: " (:search app) " (" (count (found app)) ") n next, N previous, / edit, esc ends")))
   :draw-under draw-under!
   :help       [["Search"
                 [["/" "search the text for what is typed"]
                  ["n / N" "jump to the next / previous match"]
                  ["/" "change the query"]
                  ["esc" "leave search mode"]]]]})
