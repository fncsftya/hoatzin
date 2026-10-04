(ns hoatzin.app
  "The editor as a state machine: plain-data events in, a new app out, and
  drawing the app with any SDL renderer.

  Nothing here reads the OS event queue, the clock, the clipboard or files:
  the host passes time into `handle`/`draw!` and supplies :density-fn,
  :clipboard-fn (read the clipboard), :set-clipboard-fn (write it) and
  :open-dialog-fn (show a file dialog, whose file comes back as :opened).
  That is what lets tests drive the editor headlessly and deterministically.

  The editor is modal. In :normal mode the text is left alone: keys move the
  caret and select, `i` enters :insert mode, and the caret is a block. In
  :insert mode typing edits the text, and escape goes back to :normal.
  `:` starts a command line (:command mode) in the status bar; return runs
  it, escape abandons it. Commands:
    :open                           choose a file and load it

  Events:
    {:type :text  :text s}          committed text input; in normal mode, a
                                    command
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
    {:type :move  :x x :y y}        pointer moved with no button down
    {:type :leave}                  pointer left the window
    {:type :release}                left button up
    {:type :wheel :dy lines}        scroll; positive is towards the top
    {:type :tick}                   nothing happened for `ms-until-wake`
    {:type :focus :focused? bool}
    {:type :opened :path p :text s} the file chosen in the open dialog, or
    {:type :opened :path p :error e} why it could not be read (and :path
                                    may be missing, if the dialog failed)
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
   :status-font-size 13        ; points: the status bar's mode label
   :status-padding 4           ; points above and below the label
   :margin      24             ; points
   :blink-ms    530            ; the macOS caret blink period
   :wheel-lines 3
   :scrollbar-width 14         ; points: the bar's column at the right edge
   :thumb-width 6              ; points; wider while the bar is in use
   :thumb-width-active 10
   :thumb-min   32             ; points: the shortest the thumb gets
   :autoscroll-ms 50           ; how often a drag held outside the text scrolls
   :background  [30 30 46]
   :foreground  [235 230 220]
   :selection   [76 84 128]
   :selection-unfocused [58 60 80]
   :scrollbar-track [38 38 56]
   :scrollbar-thumb [78 80 102]
   :scrollbar-thumb-active [128 132 158]
   :status-background [24 24 37]
   :status-foreground [170 168 190]})

(def mode-labels {:normal "NORMAL" :insert "INSERT"})

(defn- normalize-newlines [s]
  (-> s (str/replace "\r\n" "\n") (str/replace "\r" "\n")))

;; The app is a map:
;;   :renderer :density-fn :clipboard-fn   supplied by the host
;;   :mode                                 :normal, :insert or :command
;;   :command                              the command line's text, after `:`
;;   :message                              shown in the status bar until the
;;                                         next keystroke
;;   :path                                 the file loaded, if any
;;   :density :font                        the font, at the current density
;;   :status                               the status bar's {:font :metrics
;;                                         :lines}, :lines an atom caching
;;                                         its text, typeset
;;   :status-textures                      its label texture cache
;;   :ctx :layout :laid-out                layout context, layout, its text
;;   :textures :scratch                    line texture cache, FFI scratch
;;   :doc :goal-x                          the document; column for up/down
;;   :upstream?                            the caret, at a wrap point, is drawn
;;                                         at the end of the line above
;;   :dragging? :drag-word :drag-point     a click is extending the selection
;;                                         (by words, from :drag-word [lo hi]),
;;                                         the pointer last at :drag-point [x y]
;;   :hover? :grab                         the pointer is over the scroll bar;
;;                                         the thumb is held :grab px below its top
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

(defn- status-view
  "The status bar's font, its metrics and a cache for its typeset text."
  [app]
  (let [font (ct/font (:family app) (* (:status-font-size app) (:density app)))]
    {:font font :metrics (layout/metrics font) :lines (atom {})}))

(defn- release-status-lines! [lines]
  (run! ct/release-paragraph (vals @lines))
  (reset! lines {}))

(defn- status-text
  "What the status bar says: the command line, a message, or the mode."
  [{:keys [mode command message]}]
  (cond (= mode :command) (str ":" command)
        message           message
        :else             (mode-labels mode)))

(defn- status-line
  "The status bar's text, typeset. Only the latest text is kept: it changes
  with every keystroke on the command line."
  [{:keys [status] :as app}]
  (let [text  (status-text app)
        lines (:lines status)]
    (or (get @lines text)
        (let [p (ct/typeset (:font status) text 1.0e9)]
          (release-status-lines! lines)
          (swap! lines assoc text p)
          p))))

(defn- release-view! [{:keys [ctx textures font status status-textures]}]
  (some-> ctx layout/release-context)
  (some-> textures textures/clear!)
  (some-> font ct/release-font)
  (some-> status-textures textures/clear!)
  (when status
    (release-status-lines! (:lines status))
    (ct/release-font (:font status))))

(defn sync-view
  "Bring font, layout context and layout up to date with the renderer's
  output and the text. Each step is a cheap comparison unless its inputs
  changed."
  [app]
  (let [density (double ((:density-fn app)))
        app (if (= density (:density app))
              app
              (do (release-view! app)
                  (let [app (assoc app :density density
                                       :font (ct/font (:family app) (* (:font-size app) density))
                                       :ctx nil)]
                    (assoc app :status (status-view app)))))
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

(defn- status-height [app]
  (+ (get-in app [:status :metrics :line-height]) (* 2 (px app (:status-padding app)))))

(defn- text-height
  "The height above the status bar: the text, its margins and the scroll bar."
  [app]
  (- (second (:size app)) (status-height app)))

(defn- view-height [app] (- (text-height app) (* 2 (px app (:margin app)))))

(defn- content-height [app]
  (let [L (:layout app)] (* (layout/line-count L) (layout/line-height L))))

(defn- max-scroll [app] (max 0 (- (content-height app) (view-height app))))

(defn- clamp-scroll [app]
  (update app :scroll #(max 0 (min % (max-scroll app)))))

(defn- scroll-to [app scroll]
  (-> app (assoc :scroll (long (Math/round (double scroll)))) clamp-scroll (assoc :dirty? true)))

(defn- visible-lines
  "The visual lines [k0, k1) at least partly in view."
  [{:keys [layout scroll] :as app}]
  (let [lh (layout/line-height layout)]
    [(quot scroll lh)
     (min (layout/line-count layout) (inc (quot (+ scroll (view-height app)) lh)))]))

(defn scrollbar
  "The scroll bar in render pixels, or nil when the text fits: the bar's
  column {:x :w}, the track {:top :height}, the thumb {:thumb-y :thumb-h},
  and the :max-scroll the track's travel stands for."
  [app]
  (let [ms (max-scroll app)]
    (when (pos? ms)
      (let [[w] (:size app)
            h       (text-height app)
            inset   (px app 2)
            track   (- h (* 2 inset))
            thumb-h (min track (max (px app (:thumb-min app))
                                    (quot (* track (view-height app)) (content-height app))))
            bw      (px app (:scrollbar-width app))]
        {:x (- w bw) :w bw :top inset :height track :thumb-h thumb-h
         :thumb-y (+ inset (long (Math/round (/ (* (- track thumb-h) (double (:scroll app))) ms))))
         :max-scroll ms}))))

(defn- on-scrollbar? [app x]
  (when-let [{bx :x} (scrollbar app)] (>= x bx)))

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
  optional): :density-fn, :clipboard-fn, :set-clipboard-fn, :open-dialog-fn,
  :now, :mode (:normal unless given), and any key of `defaults`.
  Release it with `destroy!`."
  [{:keys [now] :or {now 0} :as opts}]
  (settle (merge defaults
                 {:density-fn   (constantly 1.0)
                  :clipboard-fn (constantly "")
                  :set-clipboard-fn (fn [_])
                  :open-dialog-fn (fn [])
                  :textures     (textures/cache)
                  :status-textures (textures/cache)
                  :scratch      {:frect (ffi/alloc sdl/frect) :irect (ffi/alloc sdl/rect)}
                  :doc          ed/empty-doc
                  :mode         :normal
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

(defn- insert? [app] (= :insert (:mode app)))

(defn- edit
  "Apply `f` to the document; normal mode leaves it alone."
  [app now f & args]
  (if (insert? app)
    (-> app (assoc :doc (apply f (:doc app) args) :goal-x nil :upstream? false) (touched now))
    app))

(defn- enter-mode [app now mode]
  (assoc app :mode mode :dirty? true :blink-from now))

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
                        (let [s (normalize-newlines ((:clipboard-fn app)))]
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

(defn- on-click [app now x0 y mod clicks]
  (let [[k x] (point->line app x0 y)
        word (when (>= clicks 2) (word-at app k x))]
    (-> (if-let [[lo hi] word]
          (select-range app now lo hi)
          (move-on-line app now (pos? (bit-and mod sdl/KMOD-SHIFT))
                        k (layout/position-at (:layout app) k x)))
        (assoc :dragging? true :drag-word word :drag-point [x0 y]))))

(defn- on-drag [app now x0 y]
  (let [[k x] (point->line app x0 y)
        pos (layout/position-at (:layout app) k x)
        app (assoc app :drag-point [x0 y])]
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
  (scroll-to app (- (:scroll app) (* dy (:wheel-lines app) (layout/line-height (:layout app))))))

(defn- on-scrollbar-click
  "Grab the thumb, or page towards the click on either side of it."
  [app y]
  (let [{:keys [thumb-y thumb-h]} (scrollbar app)
        lh   (layout/line-height (:layout app))
        page (max lh (- (view-height app) lh))]
    (cond (< y thumb-y)              (scroll-to app (- (:scroll app) page))
          (>= y (+ thumb-y thumb-h)) (scroll-to app (+ (:scroll app) page))
          :else                      (assoc app :grab (- y thumb-y) :dirty? true))))

(defn- on-thumb-drag [app y]
  (if-let [{:keys [top height thumb-h max-scroll]} (scrollbar app)]
    (let [travel (- height thumb-h)]
      (scroll-to app (if (pos? travel)
                       (* max-scroll (/ (- y (:grab app) top) (double travel)))
                       0)))
    app))

(defn- hover [app over?]
  (if (= over? (boolean (:hover? app)))
    app
    (assoc app :hover? over? :dirty? true)))

(defn- autoscrolling?
  "Whether a text drag is held above or below the text, which scrolls."
  [app]
  (when-let [[_ y] (and (:dragging? app) (:drag-point app))]
    (let [m (px app (:margin app))]
      (or (< y m) (>= y (+ m (view-height app)))))))

(defn- autoscroll
  "Drag again at the held point: it is further along the text now that the
  last step scrolled, so the selection grows and scrolls on."
  [app now]
  (if (autoscrolling? app)
    (let [[x y] (:drag-point app)
          app2  (on-drag app now x y)]
      (if (= (:doc app2) (:doc app)) app app2))
    app))

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

(defn- leave-command [app now]
  (-> app (enter-mode now :normal) (dissoc :command)))

(defn- run-command
  "Run the command line, back in normal mode."
  [app now]
  (let [command (str/trim (:command app))
        app     (leave-command app now)]
    (case command
      ""     app
      "open" (do ((:open-dialog-fn app)) app)
      (assoc app :message (str "Not an editor command: " command)))))

(defn- on-command-key
  "A key on the command line. Other keys leave the document alone."
  [app now key]
  (let [command (:command app)]
    (condp = key
      sdl/K-ESCAPE    (leave-command app now)
      sdl/K-RETURN    (run-command app now)
      sdl/K-KP-ENTER  (run-command app now)
      ;; Backspacing past the `:` leaves the command line.
      sdl/K-BACKSPACE (if (empty? command)
                        (leave-command app now)
                        (-> app (assoc :command (subs command 0 (dec (count command))))
                            (assoc :dirty? true :blink-from now)))
      app)))

(defn- on-text
  "Typed text: inserted in insert mode, onto the command line in command
  mode, and a command in normal mode."
  [app now text]
  ;; Normal mode reads its commands from text, not keys: the `i` key is
  ;; followed by its text, which would otherwise be typed into the insert
  ;; mode it began.
  (case (:mode app)
    :insert  (edit (dissoc app :composition) now ed/insert text)
    :command (-> app (update :command str text) (assoc :dirty? true :blink-from now))
    (case text
      "i" (enter-mode app now :insert)
      ":" (-> app (enter-mode now :command) (assoc :command ""))
      app)))

(defn- load-file
  "The file chosen to open: loaded with the caret at its start, or why not."
  [app now {:keys [path text error]}]
  (let [file (some-> path (str/split #"/") peek)]
    (if error
      (assoc app :message (str "Can't open " (or file "a file") ": " error) :dirty? true)
      (let [text (normalize-newlines text)]
        (-> app
            (assoc :doc (assoc ed/empty-doc :text text)
                   :path path :scroll 0 :goal-x nil :upstream? false
                   :message (str "\"" file "\" " (count (str/split-lines text)) " lines"))
            (dissoc :composition :dragging? :drag-word :drag-point)
            (touched now))))))

(defn handle
  "The app after `event` (see the ns doc) at time `now` (ms)."
  [app event now]
  (let [app (sync-view app)                 ; navigation needs a fresh layout
        composing? (some? (:composition app))
        ;; a message lasts until the next keystroke
        app (if (and (:message app) (#{:key :text} (:type event)))
              (-> app (dissoc :message) (assoc :dirty? true))
              app)]
    (case (:type event)
      :quit   (assoc app :quit? true)
      :text   (on-text app now (:text event))
      ;; Normal mode has no use for the input method's marked text.
      :composition (if (insert? app) (compose app now (:text event) (:cursor event)) app)
      ;; While composing, keys and clicks belong to the input method, and the
      ;; layout shows the composition, so its positions aren't the document's.
      :key    (cond composing? app
                    (= :command (:mode app)) (on-command-key app now (:key event))
                    :else (on-key app now (:key event) (:mod event 0)))
      ;; The scroll bar leaves the document alone, so it works while composing.
      :click  (cond (on-scrollbar? app (:x event)) (on-scrollbar-click app (:y event))
                    composing? app
                    :else (on-click app now (:x event) (:y event)
                                    (:mod event 0) (:clicks event 1)))
      :drag   (cond (:grab app) (on-thumb-drag app (:y event))
                    composing? app
                    :else (on-drag app now (:x event) (:y event)))
      :release (cond-> (dissoc app :dragging? :drag-word :drag-point :grab)
                 (:grab app) (assoc :dirty? true))
      :move   (hover app (boolean (on-scrollbar? app (:x event))))
      :leave  (hover app false)
      :tick   (autoscroll app now)
      :wheel  (on-wheel app (:dy event))
      :focus  (assoc app :focused? (:focused? event) :blink-from now :dirty? true)
      :opened (load-file app now event)
      :expose (assoc app :dirty? true)
      app)))

;; ---------------------------------------------------------------- drawing

(defn- block-extent
  "The block caret's [x0 x1]: over the character after the caret, or half
  an em wide where there is none on its line (at a line's end)."
  [app x k]
  (let [L    (:layout app)
        pos  (view-caret app)
        next (layout/next-position L pos)
        [x1 k1] (layout/caret L next true)]
    (if (and (< pos next) (= k k1) (not= x x1))
      [(min x x1) (max x x1)]
      [x (+ x (px app (/ (:font-size app) 2)))])))

(defn- command? [app] (= :command (:mode app)))

(defn- text-caret-rect
  "A bar in insert mode, a block in normal mode."
  [app]
  (let [{:keys [layout scroll]} app
        {:keys [line-height caret-top caret-height]} (:metrics layout)
        m (px app (:margin app))
        [x k] (caret-place app)
        y (+ m (- (* k line-height) scroll) caret-top)]
    (if (insert? app)
      [(+ m (long (Math/floor x))) y (max 1 (px app 1)) caret-height]
      (let [[x0 x1] (block-extent app x k)
            x0 (long (Math/floor x0))]
        [(+ m x0) y (max 1 (- (long (Math/ceil x1)) x0)) caret-height]))))

(defn- command-caret-rect
  "A bar at the end of the command line."
  [{:keys [status] :as app}]
  (let [{:keys [caret-top caret-height]} (:metrics status)
        {:keys [lines length]} (status-line app)
        x (ct/offset-for-index (:line (first lines)) length)]
    [(+ (px app (:margin app)) (long (Math/floor x)))
     (+ (text-height app) (px app (:status-padding app)) caret-top)
     (max 1 (px app 1))
     caret-height]))

(defn caret-rect
  "The caret's [x y w h] in render pixels. While there is a command line,
  the caret is there, not in the text."
  [app]
  (if (command? app) (command-caret-rect app) (text-caret-rect app)))

(defn- caret-in-view? [app]
  (let [[_ y _ h] (caret-rect app)
        m (px app (:margin app))]
    (and (< y (+ m (view-height app))) (> (+ y h) m))))

(defn- caret-blinking?
  "The caret shows while focused and nothing is selected; a selection
  replaces it. Scrolled out of view, it has nothing to blink. On the
  command line, it always shows."
  [app]
  (and (:focused? app)
       (or (command? app)
           (and (nil? (ed/selection (:doc app))) (caret-in-view? app)))))

(defn caret-visible? [app now]
  (and (caret-blinking? app) (even? (quot (- now (:blink-from app)) (:blink-ms app)))))

(defn ms-until-wake
  "How long the host may sleep before sending a :tick: until the caret next
  toggles or a held drag next scrolls, or indefinitely (-1) when nothing
  changes without an event."
  [app now]
  (let [b (:blink-ms app)
        blink (if (caret-blinking? app) (- b (mod (- now (:blink-from app)) b)) -1)]
    (cond (not (autoscrolling? app)) blink
          (neg? blink)               (:autoscroll-ms app)
          :else                      (min blink (:autoscroll-ms app)))))

(defn pointer
  "The mouse cursor the pointer should show: :arrow over the scroll bar,
  else :text."
  [app]
  (if (or (:hover? app) (:grab app)) :arrow :text))

(defn needs-draw? [app now]
  (or (:dirty? app) (not= (caret-visible? app now) (:drawn-phase app))))

(defn- draw-scrollbar! [app]
  (when-let [{:keys [x w top height thumb-y thumb-h]} (scrollbar app)]
    (let [{:keys [renderer scratch]} app
          frect   (:frect scratch)
          active? (or (:hover? app) (:grab app))
          tw      (px app (if active? (:thumb-width-active app) (:thumb-width app)))
          tx      (- (+ x w) (px app 2) tw)
          [tr tg tb] (:scrollbar-track app)
          [r g b] (if active? (:scrollbar-thumb-active app) (:scrollbar-thumb app))]
      (when active?
        (sdl/set-render-draw-color renderer tr tg tb 255)
        (sdl/render-fill-rect renderer (sdl/set-frect! frect x 0 w (text-height app))))
      (sdl/set-render-draw-color renderer r g b 255)
      (sdl/render-fill-rect renderer (sdl/set-frect! frect tx thumb-y tw thumb-h)))))

(defn- draw-block-caret!
  "The block caret: solid foreground, with the text under it redrawn in the
  background colour so it reads inverted. The line's texture is in the
  foreground colour, so colour-modulating it by background/foreground per
  channel gives the background."
  [app]
  (let [{:keys [renderer textures scratch layout scroll]} app
        {:keys [frect irect]} scratch
        {:keys [line-height baseline]} (:metrics layout)
        m  (px app (:margin app))
        [x y w h] (caret-rect app)
        [fr fg fb :as fore] (:foreground app)
        [_ k] (caret-place app)
        {:keys [line text]} (layout/visual-line layout k)
        ;; the part of the block in view; the text area's clip hides the rest
        top (max y m)
        bottom (min (+ y h) (+ m (view-height app)))]
    (sdl/set-render-draw-color renderer fr fg fb 255)
    (sdl/render-fill-rect renderer (sdl/set-frect! frect x y w h))
    (when (and (not (str/blank? text)) (< top bottom))
      (let [{:keys [texture width height pad] base :baseline}
            (textures/fetch! textures renderer text line fore)
            [r g b] (map (fn [b f] (min 255 (long (Math/round (* 255.0 (/ b (max 1 f)))))))
                         (:background app) fore)]
        (sdl/set-render-clip-rect renderer (sdl/set-rect! irect x top w (- bottom top)))
        (sdl/set-texture-color-mod texture r g b)
        (sdl/render-texture renderer texture ffi/null
                            (sdl/set-frect! frect (- m pad)
                                            (+ m (- (* k line-height) scroll) (- baseline base))
                                            width height))
        (sdl/set-texture-color-mod texture 255 255 255)))))

(defn- draw-bar-caret! [{:keys [renderer scratch] :as app}]
  (let [[x y w h] (caret-rect app)
        [r g b] (:foreground app)]
    (sdl/set-render-draw-color renderer r g b 255)
    (sdl/render-fill-rect renderer (sdl/set-frect! (:frect scratch) x y w h))))

(defn- draw-status-bar!
  "The bar along the bottom: the mode, a message or the command line."
  [app]
  (let [{:keys [renderer scratch status status-textures]} app
        frect (:frect scratch)
        [w] (:size app)
        top (text-height app)
        [r g b] (:status-background app)
        {:keys [line]} (first (:lines (status-line app)))
        {:keys [texture width height pad] base :baseline}
        (textures/fetch! status-textures renderer (status-text app) line
                         (:status-foreground app))]
    (sdl/set-render-draw-color renderer r g b 255)
    (sdl/render-fill-rect renderer (sdl/set-frect! frect 0 top w (status-height app)))
    (sdl/render-texture renderer texture ffi/null
                        (sdl/set-frect! frect (- (px app (:margin app)) pad)
                                        (+ top (px app (:status-padding app))
                                           (get-in status [:metrics :baseline]) (- base))
                                        width height))
    (textures/end-frame! status-textures)))

(defn draw!
  "Render the app at time `now` (ms), present it, and return the app."
  [app now]
  (let [{:keys [renderer layout scroll size textures scratch]} app
        {:keys [frect irect]} scratch
        [w _] size
        {:keys [line-height baseline]} (:metrics layout)
        m  (px app (:margin app))
        vh (view-height app)
        [first-k last-k] (visible-lines app)
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
        (doseq [[k x0 x1] (layout/selection-segments layout lo hi nl first-k last-k)
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
        (doseq [[k x0 x1] (layout/range-segments layout a (+ a (count comp)) first-k last-k)]
          (sdl/render-fill-rect renderer
                                (sdl/set-frect! frect
                                                (+ m (long (Math/floor x0)))
                                                (+ m (- (* k line-height) scroll) baseline below)
                                                (- (long (Math/ceil x1)) (long (Math/floor x0)))
                                                thickness)))))
    (when (and caret? (not (command? app)))
      (if (insert? app) (draw-bar-caret! app) (draw-block-caret! app)))
    (sdl/set-render-clip-rect renderer ffi/null)
    (draw-scrollbar! app)
    (draw-status-bar! app)
    (when (and caret? (command? app))
      (draw-bar-caret! app))
    (sdl/render-present renderer)
    (textures/end-frame! textures)
    (assoc app :dirty? false :drawn-phase caret?)))
