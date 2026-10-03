(ns hoatzin.app
  "The editor as a state machine: plain-data events in, a new app out, and
  drawing the app with any SDL renderer.

  Nothing here reads the OS event queue, the clock or the clipboard: the host
  passes time into `handle`/`draw!` and supplies :density-fn/:clipboard-fn.
  That is what lets tests drive the editor headlessly and deterministically.

  Events:
    {:type :text  :text s}          committed text input
    {:type :composition :text s :cursor i}
                                    input-method composition (marked text),
                                    e.g. the accent after option-e; "" ends it.
                                    :cursor is in code points (nil = end)
    {:type :key   :key k :mod m}    SDL keycode and modifier mask
    {:type :click :x x :y y}        left click, in render (pixel) coordinates
    {:type :wheel :dy lines}        scroll; positive is towards the top
    {:type :focus :focused? bool}
    {:type :expose}                 the window needs repainting
    {:type :quit}"
  (:require [clojure.string :as str]
            [jolt.ffi :as ffi]
            [hoatzin.coretext :as ct]
            [hoatzin.editor :as ed]
            [hoatzin.layout :as layout]
            [hoatzin.sdl :as sdl]
            [hoatzin.textures :as textures]))

(def defaults
  {:family      ct/default-family
   :font-size   20             ; points; scaled by the pixel density
   :margin      24             ; points
   :blink-ms    530            ; the macOS caret blink period
   :wheel-lines 3
   :background  [30 30 46]
   :foreground  [235 230 220]})

;; The app is a map:
;;   :renderer :density-fn :clipboard-fn   supplied by the host
;;   :density :font                        the font, at the current density
;;   :ctx :layout :laid-out                layout context, layout, its text
;;   :textures :scratch                    line texture cache, FFI scratch
;;   :doc :goal-x                          the document; column for up/down
;;   :composition                          {:text :cursor} while composing, or nil
;;   :size :scroll                         output size and scroll, in pixels
;;   :focused? :blink-from                 the caret blinks from :blink-from
;;   :dirty? :drawn-phase                  redraw needed / caret phase drawn
;;   :follow?                              scroll the caret into view
;;   :quit?
;; plus the keys of `defaults`.

(defn- px [app pts] (long (Math/round (* (double pts) (:density app)))))

;; ---------------------------------------------------------------- view

(defn- display-text
  "The text as shown: the document, plus any composition at the caret."
  [{:keys [doc composition]}]
  (if composition
    (let [{:keys [text caret]} doc]
      (str (subs text 0 caret) (:text composition) (subs text caret)))
    (:text doc)))

(defn- display-key
  "What the display text is a function of; cheap to compare."
  [{:keys [doc composition]}]
  (if composition [(:text doc) (:caret doc) (:text composition)] (:text doc)))

(defn- view-caret
  "Where the caret is drawn: inside the composition while composing."
  [{:keys [doc composition]}]
  (+ (:caret doc) (if composition (:cursor composition) 0)))

(defn- release-view! [{:keys [ctx textures font]}]
  (some-> ctx layout/release-context)
  (some-> textures textures/clear!)
  (some-> font ct/release-font))

(defn sync-view
  "Bring font, layout context and layout up to date with the renderer's
  output and the text. Each step is a cheap comparison unless its inputs
  changed."
  [app]
  (let [density (double ((:density-fn app)))
        app (if (= density (:density app))
              app
              (do (release-view! app)
                  (assoc app :density density
                             :font (ct/font (:family app) (* (:font-size app) density))
                             :ctx nil)))
        [w _ :as size] (sdl/render-output-size (:renderer app))
        wrap (max 1 (- w (* 2 (px app (:margin app)))))
        app (if (= wrap (get-in app [:ctx :width]))
              app
              (do (some-> (:ctx app) layout/release-context)
                  ;; Re-wrapping moves every line; keep the caret's in view.
                  (assoc app :ctx (layout/context (:font app) wrap) :layout nil :follow? true)))
        shown (display-key app)
        app (if (= size (:size app)) app (assoc app :size size :dirty? true))]
    (if (and (:layout app) (= shown (:laid-out app)))
      app
      (assoc app :layout (layout/layout (:ctx app) (display-text app))
                 :laid-out shown :dirty? true))))

(defn- view-height [app] (- (second (:size app)) (* 2 (px app (:margin app)))))

(defn- clamp-scroll [app]
  (let [L (:layout app)
        content (* (layout/line-count L) (layout/line-height L))]
    (update app :scroll #(max 0 (min % (- content (view-height app)))))))

(defn- follow-caret
  "Scroll just enough to bring the caret's line into view."
  [app]
  (let [L  (:layout app)
        lh (layout/line-height L)
        [_ k] (layout/caret L (view-caret app))
        top (* k lh)
        vh (view-height app)
        s  (:scroll app)]
    (assoc app :scroll (cond (< top s) top
                             (> (+ top lh) (+ s vh)) (- (+ top lh) vh)
                             :else s))))

(defn settle
  "Sync the view, then scroll as the last batch of events asked."
  [app]
  (let [app (sync-view app)]
    (clamp-scroll (if (:follow? app)
                    (-> app follow-caret (assoc :follow? false))
                    app))))

;; ---------------------------------------------------------------- lifecycle

(defn create
  "A new, empty editor drawing with `:renderer`. Options (all but :renderer
  optional): :density-fn, :clipboard-fn, :now, and any key of `defaults`.
  Release it with `destroy!`."
  [{:keys [now] :or {now 0} :as opts}]
  (settle (merge defaults
                 {:density-fn   (constantly 1.0)
                  :clipboard-fn (constantly "")
                  :textures     (textures/cache)
                  :scratch      {:frect (ffi/alloc sdl/frect) :irect (ffi/alloc sdl/rect)}
                  :doc          ed/empty-doc
                  :scroll       0
                  :focused?     true
                  :blink-from   now
                  :dirty?       true}
                 (dissoc opts :now))))

(defn destroy! [app]
  (release-view! app)
  (run! ffi/free (vals (:scratch app))))

;; ---------------------------------------------------------------- input

(defn- touched
  "After an edit or caret move: show the caret solid and keep it in view."
  [app now]
  (assoc app :follow? true :dirty? true :blink-from now))

(defn- edit [app now f & args]
  (-> app (assoc :doc (apply f (:doc app) args) :goal-x nil) (touched now)))

(defn- move-to
  ([app now pos] (move-to app now pos nil))
  ([app now pos goal-x]
   (-> app (assoc :doc (ed/move (:doc app) pos) :goal-x goal-x) (touched now))))

(defn- move-lines
  "Move the caret `n` visual lines, aiming for the remembered column."
  [app now n]
  (let [L (:layout app)
        [x k] (layout/caret L (get-in app [:doc :caret]))
        goal (or (:goal-x app) x)
        k2 (+ k n)]
    (cond
      (neg? k2) (move-to app now 0)
      (>= k2 (layout/line-count L)) (move-to app now (count (get-in app [:doc :text])))
      :else (move-to app now (layout/position-at L k2 goal) goal))))

(defn- on-key [app now key mod]
  (let [L    (:layout app)
        pos  (get-in app [:doc :caret])
        end  (count (get-in app [:doc :text]))
        cmd? (pos? (bit-and mod sdl/KMOD-GUI))
        [_ k] (layout/caret L pos)
        page (max 1 (quot (view-height app) (layout/line-height L)))]
    (condp = key
      sdl/K-BACKSPACE (cond cmd?       (edit app now ed/delete (layout/line-start L k) pos)
                            (pos? pos) (edit app now ed/delete (layout/prev-position L pos) pos)
                            :else      app)
      sdl/K-DELETE    (edit app now ed/delete pos (layout/next-position L pos))
      sdl/K-RETURN    (edit app now ed/insert "\n")
      sdl/K-KP-ENTER  (edit app now ed/insert "\n")
      sdl/K-LEFT      (move-to app now (if cmd? (layout/line-start L k) (layout/prev-position L pos)))
      sdl/K-RIGHT     (move-to app now (if cmd? (layout/line-end L k) (layout/next-position L pos)))
      sdl/K-HOME      (move-to app now (layout/line-start L k))
      sdl/K-END       (move-to app now (layout/line-end L k))
      sdl/K-UP        (if cmd? (move-to app now 0) (move-lines app now -1))
      sdl/K-DOWN      (if cmd? (move-to app now end) (move-lines app now 1))
      sdl/K-PAGEUP    (move-lines app now (- page))
      sdl/K-PAGEDOWN  (move-lines app now page)
      sdl/K-V         (if cmd?
                        (let [s (-> ((:clipboard-fn app))
                                    (str/replace "\r\n" "\n")
                                    (str/replace "\r" "\n"))]
                          (if (seq s) (edit app now ed/insert s) app))
                        app)
      app)))

(defn- on-click [app now x y]
  (let [L (:layout app)
        m (px app (:margin app))
        k (-> (long (Math/floor (/ (+ (- y m) (:scroll app)) (layout/line-height L))))
              (max 0)
              (min (dec (layout/line-count L))))]
    (move-to app now (layout/position-at L k (- x m)))))

(defn- on-wheel [app dy]
  (-> app
      (update :scroll #(- % (long (Math/round (* dy (:wheel-lines app)
                                                 (layout/line-height (:layout app)))))))
      clamp-scroll
      (assoc :dirty? true)))

(defn- compose
  "Show (or, for \"\", end) the input method's composition at the caret."
  [app now text cursor]
  (-> (if (empty? text)
        (dissoc app :composition)
        (let [n (count text)]
          (assoc app :composition
                 {:text text :cursor (if (and cursor (<= 0 cursor n)) cursor n)})))
      (touched now)))

(defn handle
  "The app after `event` (see the ns doc) at time `now` (ms)."
  [app event now]
  (let [app (sync-view app)                 ; navigation needs a fresh layout
        composing? (some? (:composition app))]
    (case (:type event)
      :quit   (assoc app :quit? true)
      :text   (edit (dissoc app :composition) now ed/insert (:text event))
      :composition (compose app now (:text event) (:cursor event))
      ;; While composing, keys and clicks belong to the input method, and the
      ;; layout shows the composition, so its positions aren't the document's.
      :key    (if composing? app (on-key app now (:key event) (:mod event 0)))
      :click  (if composing? app (on-click app now (:x event) (:y event)))
      :wheel  (on-wheel app (:dy event))
      :focus  (assoc app :focused? (:focused? event) :blink-from now :dirty? true)
      :expose (assoc app :dirty? true)
      app)))

;; ---------------------------------------------------------------- drawing

(defn caret-visible? [app now]
  (and (:focused? app) (even? (quot (- now (:blink-from app)) (:blink-ms app)))))

(defn ms-until-blink
  "How long the host may sleep: until the caret next toggles, or
  indefinitely (-1) while unfocused, when nothing changes without an event."
  [app now]
  (let [b (:blink-ms app)]
    (if (:focused? app) (- b (mod (- now (:blink-from app)) b)) -1)))

(defn needs-draw? [app now]
  (or (:dirty? app) (not= (caret-visible? app now) (:drawn-phase app))))

(defn caret-rect
  "The caret's [x y w h] in render pixels."
  [app]
  (let [{:keys [layout scroll]} app
        {:keys [line-height caret-top caret-height]} (:metrics layout)
        m (px app (:margin app))
        [x k] (layout/caret layout (view-caret app))]
    [(+ m (long (Math/floor x)))
     (+ m (- (* k line-height) scroll) caret-top)
     (max 1 (px app 1))
     caret-height]))

(defn draw!
  "Render the app at time `now` (ms), present it, and return the app."
  [app now]
  (let [{:keys [renderer layout scroll size textures scratch]} app
        {:keys [frect irect]} scratch
        [w _] size
        {:keys [line-height baseline]} (:metrics layout)
        m  (px app (:margin app))
        vh (view-height app)
        first-k (quot scroll line-height)
        last-k  (min (layout/line-count layout) (inc (quot (+ scroll vh) line-height)))
        caret?  (caret-visible? app now)
        [br bg bb] (:background app)
        [fr fg fb] (:foreground app)]
    (sdl/set-render-draw-color renderer br bg bb 255)
    (sdl/render-clear renderer)
    (sdl/set-render-clip-rect renderer (sdl/set-rect! irect 0 m w vh))
    (doseq [k (range first-k last-k)
            :let [{:keys [line text]} (layout/visual-line layout k)]
            :when (not (str/blank? text))]
      (let [{:keys [texture width height pad] base :baseline}
            (textures/fetch! textures renderer text line (:foreground app))
            y (+ m (- (* k line-height) scroll) (- baseline base))]
        (sdl/render-texture renderer texture ffi/null
                            (sdl/set-frect! frect (- m pad) y width height))))
    (when-let [{comp :text} (:composition app)]
      (sdl/set-render-draw-color renderer fr fg fb 255)
      (let [a (get-in app [:doc :caret])
            thickness (max 1 (px app 1))
            below     (max 1 (px app 2))]
        (doseq [[k x0 x1] (layout/range-segments layout a (+ a (count comp)))]
          (sdl/render-fill-rect renderer
                                (sdl/set-frect! frect
                                                (+ m (long (Math/floor x0)))
                                                (+ m (- (* k line-height) scroll) baseline below)
                                                (- (long (Math/ceil x1)) (long (Math/floor x0)))
                                                thickness)))))
    (when caret?
      (let [[x y cw ch] (caret-rect app)]
        (sdl/set-render-draw-color renderer fr fg fb 255)
        (sdl/render-fill-rect renderer (sdl/set-frect! frect x y cw ch))))
    (sdl/set-render-clip-rect renderer ffi/null)
    (sdl/render-present renderer)
    (textures/end-frame! textures)
    (assoc app :dirty? false :drawn-phase caret?)))
