(ns hoatzin.app
  "The editor as a state machine: plain-data events in, a new app out, and
  drawing the app with any SDL renderer.

  Nothing here reads the OS event queue, the clock or the clipboard: the host
  passes time into `handle`/`draw!` and supplies :density-fn, :clipboard-fn
  (read the clipboard) and :set-clipboard-fn (write it).
  That is what lets tests drive the editor headlessly and deterministically.

  Events:
    {:type :text  :text s}          committed text input
    {:type :composition :text s :cursor i}
                                    input-method composition (marked text),
                                    e.g. the accent after option-e; "" ends it.
                                    :cursor is in code points (nil = end)
    {:type :key   :key k :mod m}    SDL keycode and modifier mask
    {:type :click :x x :y y :mod m :clicks n}
                                    left button down, in render (pixel)
                                    coordinates; shift extends the selection,
                                    a double click (:clicks 2) selects a word
    {:type :drag  :x x :y y}        pointer moved with the left button down
    {:type :release}                left button up
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
   :foreground  [235 230 220]
   :selection   [76 84 128]
   :selection-unfocused [58 60 80]})

;; The app is a map:
;;   :renderer :density-fn :clipboard-fn   supplied by the host
;;   :density :font                        the font, at the current density
;;   :ctx :layout :laid-out                layout context, layout, its text
;;   :textures :scratch                    line texture cache, FFI scratch
;;   :doc :goal-x                          the document; column for up/down
;;   :upstream?                            the caret, at a wrap point, is drawn
;;                                         at the end of the line above
;;   :dragging? :drag-word                 a click is extending the selection
;;                                         (by words, from :drag-word [lo hi])
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

(defn- caret-place
  "The caret's [x visual-line], minding its affinity at a wrap point. The
  composition moves the caret off any wrap point, so ignore it then."
  [{:keys [layout composition upstream?] :as app}]
  (layout/caret layout (view-caret app) (and upstream? (nil? composition))))

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
        [_ k] (caret-place app)
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
                  :set-clipboard-fn (fn [_])
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
  (-> app (assoc :doc (apply f (:doc app) args) :goal-x nil :upstream? false) (touched now)))

(defn- move-to
  "Move the caret to `pos`, or with `extend?` extend the selection to it."
  ([app now extend? pos] (move-to app now extend? pos nil false))
  ([app now extend? pos goal-x upstream?]
   (-> app
       (assoc :doc ((if extend? ed/select ed/move) (:doc app) pos)
              :goal-x goal-x :upstream? upstream?)
       (touched now))))

(defn- move-on-line
  "Move (or extend) to `pos` on visual line `k`, keeping the caret on that
  line if `pos` is where it wraps."
  ([app now extend? k pos] (move-on-line app now extend? k pos nil))
  ([app now extend? k pos goal-x]
   (move-to app now extend? pos goal-x (layout/wrap-end? (:layout app) k pos))))

(defn- caret-or
  "[x visual-line] of `pos`, as drawn if it is the caret."
  [app pos]
  (if (= pos (get-in app [:doc :caret]))
    (caret-place app)
    (layout/caret (:layout app) pos)))

(defn- move-lines
  "Move `n` visual lines from `from`, aiming for the remembered column."
  [app now extend? from n]
  (let [L (:layout app)
        [x k] (caret-or app from)
        goal (or (when (= from (get-in app [:doc :caret])) (:goal-x app)) x)
        k2 (+ k n)]
    (cond
      (neg? k2) (move-to app now extend? 0)
      (>= k2 (layout/line-count L)) (move-to app now extend? (count (get-in app [:doc :text])))
      :else (move-on-line app now extend? k2 (layout/position-at L k2 goal) goal))))

(defn- copy! [app]
  (when-let [s (ed/selected-text (:doc app))]
    ((:set-clipboard-fn app) s)))

(defn- on-key [app now key mod]
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
      sdl/K-X         (if (and cmd? sel)
                        (do (copy! app) (edit app now ed/delete (first sel) (second sel)))
                        app)
      sdl/K-V         (if cmd?
                        (let [s (-> ((:clipboard-fn app))
                                    (str/replace "\r\n" "\n")
                                    (str/replace "\r" "\n"))]
                          (if (seq s) (edit app now ed/insert s) app))
                        app)
      app)))

(defn- point->line
  "The visual line under render pixel (x, y), and x along it: [k x]."
  [app x y]
  (let [L (:layout app)
        m (px app (:margin app))
        k (-> (long (Math/floor (/ (+ (- y m) (:scroll app)) (layout/line-height L))))
              (max 0)
              (min (dec (layout/line-count L))))]
    [k (- x m)]))

(defn- word-at
  "The [lo hi] word (or whitespace) under the point, or nil on an empty line."
  [app k x]
  (when-let [i (layout/char-at (:layout app) k x)]
    (ed/word-range (get-in app [:doc :text]) i)))

(defn- select-range
  "Select from `anchor` to `pos`."
  [app now anchor pos]
  (-> app
      (assoc :doc (ed/select (ed/move (:doc app) anchor) pos) :goal-x nil :upstream? false)
      (touched now)))

(defn- on-click [app now x y mod clicks]
  (let [[k x] (point->line app x y)
        word (when (>= clicks 2) (word-at app k x))]
    (-> (if-let [[lo hi] word]
          (select-range app now lo hi)
          (move-on-line app now (pos? (bit-and mod sdl/KMOD-SHIFT))
                        k (layout/position-at (:layout app) k x)))
        (assoc :dragging? true :drag-word word))))

(defn- on-drag [app now x y]
  (let [[k x] (point->line app x y)
        pos (layout/position-at (:layout app) k x)]
    (cond
      (not (:dragging? app)) app
      ;; After a double click, the selection grows a word at a time and
      ;; always keeps the word first clicked.
      (:drag-word app)
      (let [[lo hi] (:drag-word app)
            [wlo whi] (or (word-at app k x) [pos pos])]
        (if (< wlo lo)
          (select-range app now hi wlo)
          (select-range app now lo (max hi whi))))
      :else (move-on-line app now true k pos))))

(defn- on-wheel [app dy]
  (-> app
      (update :scroll #(- % (long (Math/round (* dy (:wheel-lines app)
                                                 (layout/line-height (:layout app)))))))
      clamp-scroll
      (assoc :dirty? true)))

(defn- compose
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
      :click  (if composing? app (on-click app now (:x event) (:y event)
                                           (:mod event 0) (:clicks event 1)))
      :drag   (if composing? app (on-drag app now (:x event) (:y event)))
      :release (dissoc app :dragging? :drag-word)
      :wheel  (on-wheel app (:dy event))
      :focus  (assoc app :focused? (:focused? event) :blink-from now :dirty? true)
      :expose (assoc app :dirty? true)
      app)))

;; ---------------------------------------------------------------- drawing

(defn- caret-blinking?
  "The caret shows while focused and nothing is selected; a selection
  replaces it."
  [app]
  (and (:focused? app) (nil? (ed/selection (:doc app)))))

(defn caret-visible? [app now]
  (and (caret-blinking? app) (even? (quot (- now (:blink-from app)) (:blink-ms app)))))

(defn ms-until-blink
  "How long the host may sleep: until the caret next toggles, or
  indefinitely (-1) when there is no blinking caret, when nothing changes
  without an event."
  [app now]
  (let [b (:blink-ms app)]
    (if (caret-blinking? app) (- b (mod (- now (:blink-from app)) b)) -1)))

(defn needs-draw? [app now]
  (or (:dirty? app) (not= (caret-visible? app now) (:drawn-phase app))))

(defn caret-rect
  "The caret's [x y w h] in render pixels."
  [app]
  (let [{:keys [layout scroll]} app
        {:keys [line-height caret-top caret-height]} (:metrics layout)
        m (px app (:margin app))
        [x k] (caret-place app)]
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
    (when-let [[lo hi] (ed/selection (:doc app))]
      (let [[r g b] (if (:focused? app) (:selection app) (:selection-unfocused app))
            nl (max 1 (quot (get-in layout [:metrics :caret-height]) 4))]
        (sdl/set-render-draw-color renderer r g b 255)
        (doseq [[k x0 x1] (layout/selection-segments layout lo hi nl)
                :let [x0 (long (Math/floor x0))]]
          (sdl/render-fill-rect renderer
                                (sdl/set-frect! frect (+ m x0) (+ m (- (* k line-height) scroll))
                                                (- (long (Math/ceil x1)) x0) line-height)))))
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
