(ns hoatzin.app
  "The editor as a state machine: plain-data events in, a new app out, and
  drawing the app with any SDL renderer.

  Nothing here reads the OS event queue, the clock, the clipboard or files:
  the host passes time into `handle`/`draw!` and supplies :density-fn,
  :clipboard-fn (read the clipboard), :set-clipboard-fn (write it),
  :open-dialog-fn (show a file dialog, whose file comes back as :opened),
  :save-dialog-fn (show a save dialog starting at the path it is given, or
  nil; the choice comes back as :save-chosen) and :write-file-fn (write
  string s to path p, returning nil, or why it could not).
  That is what lets tests drive the editor headlessly and deterministically.

  The editor is modal. In :normal mode the text is left alone: keys move the
  caret and select, `i` enters :insert mode, and the caret is a block. In
  :insert mode typing edits the text, and escape goes back to :normal.
  `:` starts a command line (:command mode) in the status bar; return runs
  it, escape abandons it, and tab completes the command's name. While it is
  open, a box above the status bar lists the commands that what is typed
  could still complete to. A command
  runs from any prefix that begins no other, so `:w` is `:write`. Commands:
    :open                           choose a file and load it
    :write                          save the file (choosing where, if it
                                    has no path yet)
    :save                           choose where to save the file, and save it
    :quit                           quit the editor, unless there are unsaved
                                    changes; `:quit!` quits regardless
    :settings                       show the settings window, over the text
                                    until escape closes it

  While a window is open, it takes the input: the text is left alone.

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
                                    a double click (:clicks 2) selects a word.
                                    A box (see `add-block`, `set-floats`)
                                    takes the clicks on it
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
    {:type :save-chosen :path p}    where the save dialog chose to save, or
    {:type :save-chosen :error e}   why the dialog failed
    {:type :expose}                 the window needs repainting
    {:type :quit}"
  (:require [clojure.string :as str]
            [jolt.ffi :as ffi]
            [hoatzin.coretext :as ct]
            [hoatzin.editor :as ed]
            [hoatzin.layout :as layout]
            [hoatzin.sdl :as sdl]
            [hoatzin.text :as text]
            [hoatzin.textures :as textures]
            [hoatzin.ui :as ui]))

(def defaults
  ;; Two fonts: the editor's, for the text, and the UI's, for everything
  ;; else (the status bar, the command line and boxes).
  {:editor-family ct/default-family
   :editor-font-size 20        ; points; scaled by the pixel density
   :ui-family   "Menlo"        ; monospace, and on every macOS install
   :ui-font-size 13            ; points
   :status-padding 4           ; points above and below the status bar's text
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
   :status-foreground [170 168 190]
   :ui-border   [96 98 128]    ; boxes' colours, where their style sets none
   :ui-accent   [128 132 158]
   :ui-field-background [24 24 37]})

(def mode-labels {:normal "NORMAL" :insert "INSERT"})

(defn- normalize-newlines [s]
  ;; most text has no \r: find that out with a scan that copies nothing
  (if (str/includes? s "\r")
    (-> s (str/replace "\r\n" "\n") (str/replace "\r" "\n"))
    s))

;; The app is a map:
;;   :renderer :density-fn :clipboard-fn   supplied by the host
;;   :mode                                 :normal, :insert or :command
;;   :command                              the command line's text, after `:`
;;   :message                              shown in the status bar until the
;;                                         next keystroke
;;   :path                                 the file loaded or saved, if any
;;   :saved                                the text as it is in that file
;;   :modified? :compared                  whether the text differs from
;;                                         :saved, as of :compared [text saved]
;;   :density :font                        the editor font, at the current
;;                                         density
;;   :ui :ui-textures                      the UI font's {:font :metrics
;;                                         :lines}, :lines an atom caching
;;                                         texts set as lines in it, and its
;;                                         texture cache
;;   :ctx :layout :laid-out                layout context, layout, its text
;;   :textures :scratch                    line texture cache, FFI scratch
;;   :doc :goal-x                          the document (see hoatzin.editor);
;;                                         column for up/down
;;   :upstream?                            the caret, at a wrap point, is drawn
;;                                         at the end of the line above
;;   :dragging? :drag-word :drag-point     a click is extending the selection
;;                                         (by words, from :drag-word [lo hi]),
;;                                         the pointer last at :drag-point [x y]
;;   :hover? :grab                         the pointer is over the scroll bar;
;;                                         the thumb is held :grab px below its top
;;   :composition                          {:text :cursor} while composing, or nil
;;   :window                               the window open over the text
;;                                         (:settings), or nil
;;   :blocks                               boxes in the text (see hoatzin.ui), by
;;                                         id: each sits below the paragraph
;;                                         holding the document's mark of that id
;;   :floats                               boxes above everything, placed in
;;                                         the window, last on top
;;   :ui-values                            interactive boxes' values, by :id
;;   :block-places :float-places           where they are, as `place-blocks` and
;;                                         `place-floats` say; the floats placed
;;                                         include the command line's hints
;;   :ui-hover?                            the pointer is over a box
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
      (text/insert text caret (:text composition)))
    (:text doc)))

(defn- display-key
  "What the display text is a function of: see `same-display?`."
  [{:keys [doc composition]}]
  (if composition [(:text doc) (:caret doc) (:text composition)] (:text doc)))

(defn- same-display?
  "Whether display keys `a` and `b` show the same text. The document's text
  compares by identity: every edit makes a new one, and comparing contents
  could walk the whole document."
  [a b]
  (if (vector? a)
    (and (vector? b) (identical? (a 0) (b 0)) (= (subvec a 1) (subvec b 1)))
    (identical? a b)))

(defn- view-caret
  "Where the caret is drawn: inside the composition while composing."
  [{:keys [doc composition]}]
  (+ (:caret doc) (if composition (:cursor composition) 0)))

(defn- caret-place
  "The caret's [x visual-line], minding its affinity at a wrap point. The
  composition moves the caret off any wrap point, so ignore it then."
  [{:keys [layout composition upstream?] :as app}]
  (layout/caret layout (view-caret app) (and upstream? (nil? composition))))

(defn- face
  "A font of `family` at `size` points, its metrics, and a cache of up to
  `kept` texts set as lines in it."
  [app family size kept]
  (let [font (ct/font family (* size (:density app)))]
    {:font font :metrics (layout/metrics font) :lines (atom {}) :kept kept}))

(defn- release-face-lines! [{:keys [lines]}]
  (run! #(some-> (:line %) ct/release) (vals @lines))
  (reset! lines {}))

(defn- release-face! [f]
  (release-face-lines! f)
  (ct/release-font (:font f)))

(defn- face-line
  "`text` set as a line in face `f`, as hoatzin.coretext's line functions
  take it, with its UTF-16 :length; :line is nil for \"\". Once the face
  has `kept` texts set, they are all dropped: the command line's text
  changes with every keystroke."
  [f text]
  (let [lines (:lines f)]
    (or (get @lines text)
        (let [ln {:line (ct/make-line (:font f) text) :base 0
                  :length (ct/utf16-length text)}]
          (when (>= (count @lines) (:kept f)) (release-face-lines! f))
          (swap! lines assoc text ln)
          ln))))

(defn- face-width
  "How wide `text` is in face `f`, in render pixels."
  [f text]
  (let [{:keys [line length] :as ln} (face-line f text)]
    (if line (long (Math/ceil (ct/offset-for-index ln length))) 0)))

(defn- file-name [path] (some-> path (str/split #"/") peek))

(defn- status-text
  "What the status bar says on its left: the command line, a message, or
  the mode."
  [{:keys [mode command message]}]
  (cond (= mode :command) (str ":" command)
        message           message
        :else             (mode-labels mode)))

(defn- status-file
  "What the status bar says on its right: the file's name, and [+] while
  the text differs from what is in it."
  [{:keys [path modified?]}]
  (str (or (file-name path) "[No Name]") (when modified? " [+]")))

(def ^:private ui-lines-kept
  "How many texts in the UI font are kept set as lines: more than are on
  screen."
  512)

(defn- sync-modified
  "Whether the text differs from the file, compared only when either has
  changed since last time. Texts share the lines an edit left alone, which
  compare by identity, so even then this costs little."
  [{:keys [doc saved compared] :as app}]
  (let [text (:text doc)]
    (if (and (identical? text (first compared)) (identical? saved (second compared)))
      app
      (let [modified? (not= text saved)]
        (cond-> (assoc app :compared [text saved] :modified? modified?)
          (not= modified? (:modified? app)) (assoc :dirty? true))))))

(defn- release-view! [{:keys [ctx textures font ui ui-textures]}]
  (some-> ctx layout/release-context)
  (some-> textures textures/clear!)
  (some-> font ct/release-font)
  (some-> ui-textures textures/clear!)
  (some-> ui release-face!))

(defn- ui-width
  "How wide `text` is in the UI font, in render pixels."
  [app text]
  (face-width (:ui app) text))

(defn- ui-context
  "What hoatzin.ui places boxes with: text one line high in the UI font."
  [app]
  {:scale (:density app)
   :text-size (fn [s] [(ui-width app s) (get-in app [:ui :metrics :line-height])])})

(defn- shown-pos
  "Where document position `pos` is in the display text: past the
  composition, if that is inserted before it."
  [{:keys [doc composition]} pos]
  (if (and composition (> pos (:caret doc))) (+ pos (count (:text composition))) pos))

(defn- place-blocks
  "Every block with a mark in the document, in order down the text, as
  {:id :pos :line :top :height :placed}: below visual line :line, the last
  of the paragraph holding its mark, as wide as the text. :top and :placed
  (from hoatzin.ui/place) are in content pixels: from the text column's
  left and the top of the text as scrolled."
  [app]
  (let [L      (:layout app)
        lh     (layout/line-height L)
        marks  (get-in app [:doc :marks])
        width  (get-in app [:ctx :width])
        ctx    (ui-context app)
        blocks (->> (:blocks app)
                    (keep (fn [[id node]]
                            (when-let [pos (get marks id)]
                              (let [pos (shown-pos app pos)]
                                {:id id :node node :pos pos :line (layout/last-line L pos)}))))
                    (sort-by (juxt :line :pos)))]
    (loop [bs blocks, extra 0, out []]
      (if-let [{:keys [node line] :as b} (first bs)]
        (let [h (second (ui/measure ctx node))
              top (+ (* (inc line) lh) extra)]
          (recur (next bs) (+ extra h)
                 (conj out (-> (dissoc b :node)
                               (assoc :top top :height h
                                      :placed (ui/place ctx node [0 top width h]))))))
        out))))

(defn- ui-value
  "What interactive box `node` holds: what it was set to, else its :value."
  [app {:keys [id value]}]
  (get (:ui-values app) id value))

(declare command-hints settings-window)

(defn- place-floats
  "The floats placed in the window, in render pixels: in a box as big as
  it, which lays out those in flow down its left edge. The open window, if
  any, then the command line's hints, if any, go on top."
  [app]
  (if-let [floats (seq (cond-> (:floats app)
                         (= :settings (:window app)) (conj (settings-window app))
                         (command-hints app)         (conj (command-hints app))))]
    (let [[w h] (:size app)]
      (subvec (ui/place (ui-context app)
                        {:kind :box :style {:align :start} :children (vec floats)}
                        [0 0 w h])
              1))
    []))

(defn sync-view
  "Bring font, layout context and layout up to date with the renderer's
  output and the text, then place the boxes. Each step is a cheap
  comparison unless its inputs changed. Between frames, as this is, it also
  trims the layout's cache."
  [app]
  (let [density (double ((:density-fn app)))
        app (if (= density (:density app))
              app
              (do (release-view! app)
                  (let [app (assoc app :density density
                                       :font (ct/font (:editor-family app)
                                                      (* (:editor-font-size app) density))
                                       :ctx nil)]
                    (assoc app :ui (face app (:ui-family app) (:ui-font-size app) ui-lines-kept)))))
        [w _ :as size] (sdl/render-output-size (:renderer app))
        wrap (max 1 (- w (* 2 (px app (:margin app)))))
        app (if (= wrap (get-in app [:ctx :width]))
              app
              (do (some-> (:ctx app) layout/release-context)
                  ;; Re-wrapping moves every line; keep the caret's in view.
                  (assoc app :ctx (layout/context (:font app) wrap) :layout nil :follow? true)))
        shown (display-key app)
        app (if (= size (:size app)) app (assoc app :size size :dirty? true))
        app (sync-modified app)]
    (layout/trim! (:ctx app))
    (let [app (if (and (:layout app) (same-display? shown (:laid-out app)))
                app
                (assoc app :layout (layout/layout (:ctx app) (display-text app))
                           :laid-out shown :dirty? true))]
      (assoc app :block-places (place-blocks app) :float-places (place-floats app)))))

(defn- status-height [app]
  (+ (get-in app [:ui :metrics :line-height]) (* 2 (px app (:status-padding app)))))

(defn- text-height
  "The height above the status bar: the text, its margins and the scroll bar."
  [app]
  (- (second (:size app)) (status-height app)))

(defn- view-height [app] (- (text-height app) (* 2 (px app (:margin app)))))

(defn- line-top
  "Where visual line `k` starts, in content pixels: after the lines and
  the blocks above it."
  [app k]
  (reduce (fn [y {:keys [line height]}] (if (< line k) (+ y height) (reduced y)))
          (* k (layout/line-height (:layout app)))
          (:block-places app)))

(defn- line-at-y
  "The visual line at content pixel `y`, clamped to the text. In a block,
  the line above it."
  [app y]
  (let [L  (:layout app)
        lh (layout/line-height L)
        k  (loop [bs (:block-places app), extra 0]
             (if-let [{:keys [line top height]} (first bs)]
               (cond (< y top)            (Math/floor (/ (double (- y extra)) lh))
                     (< y (+ top height)) line
                     :else                (recur (next bs) (+ extra height)))
               (Math/floor (/ (double (- y extra)) lh))))]
    (-> (long k) (max 0) (min (dec (layout/line-count L))))))

(defn- content-height [app]
  (let [L (:layout app)]
    (reduce + (* (layout/line-count L) (layout/line-height L))
            (map :height (:block-places app)))))

(defn- max-scroll [app] (max 0 (- (content-height app) (view-height app))))

(defn- clamp-scroll [app]
  (update app :scroll #(max 0 (min % (max-scroll app)))))

(defn- scroll-to [app scroll]
  (-> app (assoc :scroll (long (Math/round (double scroll)))) clamp-scroll (assoc :dirty? true)))

(defn- visible-lines
  "The visual lines [k0, k1) at least partly in view."
  [{:keys [layout scroll] :as app}]
  [(line-at-y app scroll)
   (min (layout/line-count layout) (inc (line-at-y app (+ scroll (view-height app)))))])

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
        top (line-top app k)
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
  :save-dialog-fn, :write-file-fn, :now, :mode (:normal unless given), and
  any key of `defaults`.
  Release it with `destroy!`."
  [{:keys [now] :or {now 0} :as opts}]
  (settle (merge defaults
                 {:density-fn   (constantly 1.0)
                  :clipboard-fn (constantly "")
                  :set-clipboard-fn (fn [_])
                  :open-dialog-fn (fn [])
                  :save-dialog-fn (fn [_])
                  :write-file-fn (fn [_ _] "no file system")
                  :textures     (textures/cache)
                  :ui-textures  (textures/cache)
                  :blocks       {}
                  :floats       []
                  :ui-values    {}
                  :scratch      {:frect (ffi/alloc sdl/frect) :irect (ffi/alloc sdl/rect)}
                  :doc          ed/empty-doc
                  :saved        (:text ed/empty-doc)
                  :mode         :normal
                  :scroll       0
                  :focused?     true
                  :blink-from   now
                  :dirty?       true}
                 (dissoc opts :now))))

(defn destroy! [app]
  (release-view! app)
  (run! ffi/free (vals (:scratch app))))

;; ---------------------------------------------------------------- boxes

(defn add-block
  "Show box `node` (see hoatzin.ui) in the text, below the paragraph
  holding position `pos` and as wide as the text, which makes room for it.
  It keeps to that paragraph as the text is edited. Replaces any block
  `id` was."
  [app id pos node]
  (-> app
      (update :doc ed/mark id pos)
      (assoc-in [:blocks id] node)
      (assoc :dirty? true)))

(defn remove-block [app id]
  (-> app (update :doc ed/unmark id) (update :blocks dissoc id) (assoc :dirty? true)))

(defn set-floats
  "Show boxes `nodes` above everything, placed in the window: those in
  flow down its left edge, absolute ones where they say. Later ones are
  drawn over earlier ones."
  [app nodes]
  (assoc app :floats (vec nodes) :dirty? true))

(defn set-ui-value
  "Set what the interactive box `id` holds."
  [app id value]
  (-> app (assoc-in [:ui-values id] value) (assoc :dirty? true)))

;; ---------------------------------------------------------------- input

(defn- touched
  "After an edit or caret move: show the caret solid and keep it in view."
  [app now]
  (assoc app :follow? true :dirty? true :blink-from now))

(defn- insert? [app] (= :insert (:mode app)))
(defn- command? [app] (= :command (:mode app)))

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
  (let [m (px app (:margin app))]
    [(line-at-y app (+ (- y m) (:scroll app))) (- x m)]))

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

(defn- ui-hit
  "The box under render pixel (x, y), as hoatzin.ui/hit gives it: floats
  before blocks, and blocks only where the text is in view."
  [app x y]
  (or (ui/hit (:float-places app) x y)
      (let [m (px app (:margin app))]
        (when (and (>= y m) (< y (+ m (view-height app))))
          (some #(ui/hit (:placed %) (- x m) (+ (- y m) (:scroll app)))
                (:block-places app))))))

(defn- on-ui-click
  "A click on a box: it doesn't reach the text. A checkbox toggles."
  [app {:keys [node]}]
  (case (:kind node)
    :checkbox (-> app
                  (assoc-in [:ui-values (:id node)] (not (ui-value app node)))
                  (assoc :dirty? true))
    app))

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

(defn- file-lines
  "How many lines `t` has, as an editor counts them: a final newline ends
  the last line rather than starting another."
  [t]
  (let [n (text/line-count t)]
    (if (and (> n 1) (= "" (text/line t (dec n)))) (dec n) n)))

(defn- write-file
  "Write the text to `path`, which is then the file it is the text of."
  [app path]
  (let [t (get-in app [:doc :text])
        file (file-name path)]
    (assoc (if-let [error ((:write-file-fn app) path (str t))]
             (assoc app :message (str "Can't write " file ": " error))
             (assoc app :path path :saved t
                        :message (str "\"" file "\" " (file-lines t) " lines written")))
           :dirty? true)))

(defn- save-as
  "Ask where to save; the answer comes back as :save-chosen."
  [app]
  ((:save-dialog-fn app) (:path app))
  app)

(defn- quit [app force?]
  (if (and (:modified? app) (not force?))
    (assoc app :message "Unsaved changes (add ! to override)")
    (assoc app :quit? true)))

;; Each command is (fn [app force?]), `force?` being a trailing `!`.
(def ^:private commands
  {"open"  (fn [app _] ((:open-dialog-fn app)) app)
   "write" (fn [app _] (if-let [path (:path app)] (write-file app path) (save-as app)))
   "save"  (fn [app _] (save-as app))
   "quit"  quit
   "settings" (fn [app _] (assoc app :window :settings :dirty? true))})

(defn- command-names
  "The commands `typed` could mean: the one it names, else those it begins."
  [typed]
  (if (contains? commands typed)
    [typed]
    (filterv #(str/starts-with? % typed) (sort (keys commands)))))

(defn- parse-command
  "The command line as [command force?], `force?` being a trailing `!`."
  [line]
  (let [typed  (str/trim line)
        force? (str/ends-with? typed "!")]
    [(str/trim (cond-> typed force? (subs 0 (dec (count typed))))) force?]))

(defn- run-command
  "Run the command line, back in normal mode."
  [app now]
  (let [[command force?] (parse-command (:command app))
        app     (leave-command app now)
        names   (command-names command)]
    (cond
      (= "" command)     app
      (= 1 (count names)) ((commands (first names)) app force?)
      (seq names)        (assoc app :message (str "Ambiguous command: " command
                                                  " (" (str/join ", " names) ")"))
      :else              (assoc app :message (str "Not an editor command: " command)))))

(def ^:private hint-gap "Points between the columns of command hints." 16)
(def ^:private hint-padding "Points above and below the command hints." 4)
(def ^:private hint-space "Points between the command hints and the status bar." 4)
(def ^:private hint-rows "The most rows of command hints shown." 2)

(defn- hint-columns
  "`names` in columns, read down each and then across, filling no more
  than `hint-rows` rows: on one row while they fit across `avail` pixels
  in columns `col-w` wide and `gap` apart, else on as many columns as fit,
  leaving out those that don't."
  [names col-w gap avail]
  (let [fit  (max 1 (quot (+ avail gap) (+ col-w gap)))
        rows (if (<= (count names) fit) 1 hint-rows)]
    (mapv vec (partition-all rows (take (* rows fit) names)))))

(defn- command-hints
  "While the command line is open, a float across the window just above
  the status bar, listing the commands that what is typed begins,
  alphabetically, in line with the status bar's text; nil
  when it begins none."
  [app]
  (when (command? app)
    (let [[typed] (parse-command (:command app))
          names (filterv #(str/starts-with? % typed) (sort (keys commands)))]
      (when (seq names)
        (let [d       (:density app)
              [w]     (:size app)
              m       (px app (:margin app))
              col-w   (reduce max (map #(ui-width app %) names))
              columns (hint-columns names col-w (px app hint-gap) (- w (* 2 m)))
              label   (fn [name] {:kind :label :text name
                                  :style {:color (:status-foreground app)}})]
          ;; The side padding, with the border, is the margin: the names
          ;; line up with the command line's text.
          {:kind :box
           :style {:position :absolute :left 0 :right 0
                   :bottom (+ (/ (status-height app) d) hint-space)
                   :direction :row :gap hint-gap :border 1
                   :padding [hint-padding (- (:margin app) 1)]
                   :background (:status-background app) :border-color (:ui-border app)}
           :children (mapv (fn [col] {:kind :box :style {:width (/ col-w d)}
                                      :children (mapv label col)})
                           columns)})))))

(def ^:private settings-padding "Points inside the settings window's border." 16)
(def ^:private settings-label-width "Points for the settings' labels." 100)
(def ^:private settings-size-width "Points for the settings' font size fields." 40)

(defn- settings-window
  "The settings, as a form over the text, inset by the margin. Read-only
  for now: what it shows is the app's, not what is in its fields."
  [app]
  (let [m     (:margin app)
        row   (fn [label & fields]
                {:kind :box :style {:direction :row :align :center :gap 8}
                 :children (into [{:kind :label :text label
                                   :style {:width settings-label-width}}]
                                 fields)})
        field (fn [id value & [width]]
                (cond-> {:kind :field :id id :value (str value)}
                  width (assoc :style {:width width})))
        font  (fn [label id family size]
                (row label
                     (field (keyword "settings" (str id "-family")) family)
                     (field (keyword "settings" (str id "-size")) size settings-size-width)))]
    {:kind :box
     :style {:position :absolute :left m :top m :right m
             :bottom (+ (/ (status-height app) (:density app)) m)
             :padding settings-padding :gap 10 :border 1
             :background (:status-background app) :border-color (:ui-border app)}
     :children [{:kind :label :text "Settings" :style {:color (:status-foreground app)}}
                (font "Editor font" "editor" (:editor-family app) (:editor-font-size app))
                (font "UI font" "ui" (:ui-family app) (:ui-font-size app))
                (row "Theme" (field :settings/theme "default"))
                (row "Line height" (field :settings/line-height layout/line-spacing
                                          settings-size-width))]}))

(defn- on-window-event
  "An event while a window is open: escape closes it, and clicks go to its
  boxes. The text takes nothing."
  [app event]
  (case (:type event)
    :key   (if (= sdl/K-ESCAPE (:key event)) (assoc app :window nil :dirty? true) app)
    :click (if-let [hit (ui-hit app (:x event) (:y event))] (on-ui-click app hit) app)
    (:text :composition :drag :wheel) app
    nil))

(defn- shared-start [a b]
  (subs a 0 (count (take-while true? (map = a b)))))

(defn- complete
  "The command line completed as far as the commands it could mean agree."
  [command]
  (let [names (command-names (str/triml command))]
    (if (seq names) (reduce shared-start names) command)))

(defn- on-command-key
  "A key on the command line. Other keys leave the document alone."
  [app now key]
  (let [command (:command app)]
    (condp = key
      sdl/K-ESCAPE    (leave-command app now)
      sdl/K-RETURN    (run-command app now)
      sdl/K-KP-ENTER  (run-command app now)
      sdl/K-TAB       (assoc app :command (complete command) :dirty? true :blink-from now)
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
  (let [file (file-name path)]
    (if error
      (assoc app :message (str "Can't open " (or file "a file") ": " error) :dirty? true)
      (let [t (text/of (normalize-newlines text))]
        (-> app
            (assoc :doc (assoc ed/empty-doc :text t)
                   :path path :saved t :scroll 0 :goal-x nil :upstream? false
                   :blocks {}
                   :message (str "\"" file "\" " (file-lines t) " lines"))
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
    (if-let [app (and (:window app) (on-window-event app event))]
      app
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
        ;; Boxes take the clicks on them, over the text and the scroll bar.
        :click  (if-let [hit (ui-hit app (:x event) (:y event))]
                  (on-ui-click app hit)
                  (cond (on-scrollbar? app (:x event)) (on-scrollbar-click app (:y event))
                        composing? app
                        :else (on-click app now (:x event) (:y event)
                                        (:mod event 0) (:clicks event 1))))
        :drag   (cond (:grab app) (on-thumb-drag app (:y event))
                      composing? app
                      :else (on-drag app now (:x event) (:y event)))
        :release (cond-> (dissoc app :dragging? :drag-word :drag-point :grab)
                   (:grab app) (assoc :dirty? true))
        :move   (-> (hover app (boolean (on-scrollbar? app (:x event))))
                    (assoc :ui-hover? (some? (ui-hit app (:x event) (:y event)))))
        :leave  (-> (hover app false) (dissoc :ui-hover?))
        :tick   (autoscroll app now)
        :wheel  (on-wheel app (:dy event))
        :focus  (assoc app :focused? (:focused? event) :blink-from now :dirty? true)
        :opened (load-file app now event)
        :save-chosen (if-let [error (:error event)]
                       (assoc app :message (str "Can't save: " error) :dirty? true)
                       (write-file app (:path event)))
        :expose (assoc app :dirty? true)
        app))))

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
      [x (+ x (px app (/ (:editor-font-size app) 2)))])))

(defn- text-caret-rect
  "A bar in insert mode, a block in normal mode."
  [app]
  (let [{:keys [layout scroll]} app
        {:keys [caret-top caret-height]} (:metrics layout)
        m (px app (:margin app))
        [x k] (caret-place app)
        y (+ m (- (line-top app k) scroll) caret-top)]
    (if (insert? app)
      [(+ m (long (Math/floor x))) y (max 1 (px app 1)) caret-height]
      (let [[x0 x1] (block-extent app x k)
            x0 (long (Math/floor x0))]
        [(+ m x0) y (max 1 (- (long (Math/ceil x1)) x0)) caret-height]))))

(defn- command-caret-rect
  "A bar at the end of the command line."
  [{:keys [ui] :as app}]
  (let [{:keys [caret-top caret-height]} (:metrics ui)
        {:keys [length] :as ln} (face-line ui (status-text app))
        x (ct/offset-for-index ln length)]
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
  "The caret shows while focused, with no window open, and nothing
  selected; a selection
  replaces it. Scrolled out of view, it has nothing to blink. On the
  command line, it always shows."
  [app]
  (and (:focused? app)
       (not (:window app))
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
  "The mouse cursor the pointer should show: :arrow over the scroll bar
  or a box, or while a window is open, else :text."
  [app]
  (if (or (:hover? app) (:grab app) (:ui-hover? app) (:window app)) :arrow :text))

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
        {:keys [baseline]} (:metrics layout)
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
                                            (+ m (- (line-top app k) scroll) (- baseline base))
                                            width height))
        (sdl/set-texture-color-mod texture 255 255 255)))))

(defn- draw-bar-caret! [{:keys [renderer scratch] :as app}]
  (let [[x y w h] (caret-rect app)
        [r g b] (:foreground app)]
    (sdl/set-render-draw-color renderer r g b 255)
    (sdl/render-fill-rect renderer (sdl/set-frect! (:frect scratch) x y w h))))

(defn- draw-status-text!
  "`text` in the status bar, starting at render pixel `x`."
  [app text x]
  (let [{:keys [renderer scratch ui ui-textures]} app
        color (:status-foreground app)
        {:keys [line]} (face-line ui text)
        {:keys [texture width height pad] base :baseline}
        (textures/fetch! ui-textures renderer [color text] line color)]
    (sdl/render-texture renderer texture ffi/null
                        (sdl/set-frect! (:frect scratch) (- x pad)
                                        (+ (text-height app) (px app (:status-padding app))
                                           (get-in ui [:metrics :baseline]) (- base))
                                        width height))))

(defn- draw-status-bar!
  "The bar along the bottom: the mode, a message or the command line on
  the left, and the file on the right, unless the left runs into it."
  [app]
  (let [{:keys [renderer scratch]} app
        [w] (:size app)
        m (px app (:margin app))
        [r g b] (:status-background app)
        left  (status-text app)
        right (status-file app)
        right-x (- w m (ui-width app right))]
    (sdl/set-render-draw-color renderer r g b 255)
    (sdl/render-fill-rect renderer (sdl/set-frect! (:frect scratch) 0 (text-height app)
                                                   w (status-height app)))
    (draw-status-text! app left m)
    (when (< (+ m (ui-width app left) m) right-x)
      (draw-status-text! app right right-x))))

(defn- set-clip!
  "Clip drawing to [x y w h], or with nil not at all."
  [{:keys [renderer scratch]} clip]
  (sdl/set-render-clip-rect renderer (if-let [[x y w h] clip]
                                       (sdl/set-rect! (:irect scratch) x y w h)
                                       ffi/null)))

(defn- intersect
  "Rects [x y w h] `a` and `b` (nil: everywhere) overlap here, or nil."
  [a b]
  (if-not a
    b
    (let [[ax ay aw ah] a, [bx by bw bh] b
          x0 (max ax bx), y0 (max ay by)
          x1 (min (+ ax aw) (+ bx bw)), y1 (min (+ ay ah) (+ by bh))]
      (when (and (< x0 x1) (< y0 y1)) [x0 y0 (- x1 x0) (- y1 y0)]))))

(defn- draw-ui-text!
  "One line of `text` in `rect` [x y w h], in the UI font, clipped to the
  rect and to `clip`: centred vertically, and with `centre?` horizontally."
  [app text [x y w h :as rect] color centre? clip]
  (when-let [visible (and (seq text) (intersect clip rect))]
    (let [{:keys [renderer scratch ui ui-textures]} app
          {:keys [line-height baseline]} (:metrics ui)
          {:keys [line]} (face-line ui text)
          {:keys [texture width height pad] base :baseline}
          (textures/fetch! ui-textures renderer [color text] line color)
          x (if centre? (+ x (quot (- w (face-width ui text)) 2)) x)]
      (set-clip! app visible)
      (sdl/render-texture renderer texture ffi/null
                          (sdl/set-frect! (:frect scratch) (- x pad)
                                          (+ y (quot (- h line-height) 2) baseline (- base))
                                          width height))
      (set-clip! app clip))))

(defn- draw-boxes!
  "Placed boxes (see hoatzin.ui/place) moved by (`dx`, `dy`), within
  `clip` [x y w h] (nil: the whole window): each its background, its
  border, then what it holds."
  [app placed dx dy clip]
  (let [{:keys [renderer scratch]} app
        fill! (fn [[r g b] x y w h]
                (sdl/set-render-draw-color renderer r g b 255)
                (sdl/render-fill-rect renderer (sdl/set-frect! (:frect scratch) x y w h)))]
    (doseq [{:keys [node] [x y w h] :rect [cx cy cw ch] :content} placed
            :let [st (ui/style node)
                  x (+ x dx), y (+ y dy), cx (+ cx dx), cy (+ cy dy)
                  bw (px app (:border st 0))
                  border (or (:border-color st) (:ui-border app))
                  color (or (:color st) (:foreground app))
                  bg (or (:background st) (when (= :field (:kind node)) (:ui-field-background app)))]]
      (when bg (fill! bg x y w h))
      (when (pos? bw)
        (fill! border x y w bw)
        (fill! border x (- (+ y h) bw) w bw)
        (fill! border x (+ y bw) bw (- h bw bw))
        (fill! border (- (+ x w) bw) (+ y bw) bw (- h bw bw)))
      (case (:kind node)
        :label    (draw-ui-text! app (str (:text node)) [cx cy cw ch] color false clip)
        :button   (draw-ui-text! app (str (:text node)) [cx cy cw ch] color true clip)
        :field    (draw-ui-text! app (str (ui-value app node)) [cx cy cw ch] color false clip)
        :checkbox (when (ui-value app node)
                    (fill! (or (:color st) (:ui-accent app)) cx cy cw ch))
        nil))))

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
                                (sdl/set-frect! frect (+ m x0) (+ m (- (line-top app k) scroll))
                                                (- (long (Math/ceil x1)) x0) line-height)))))
    (doseq [k (range first-k last-k)
            :let [{:keys [line text]} (layout/visual-line layout k)]
            :when (not (str/blank? text))]
      (let [{:keys [texture width height pad] base :baseline}
            (textures/fetch! textures renderer text line (:foreground app))
            y (+ m (- (line-top app k) scroll) (- baseline base))]
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
                                                (+ m (- (line-top app k) scroll) baseline below)
                                                (- (long (Math/ceil x1)) (long (Math/floor x0)))
                                                thickness)))))
    (doseq [{:keys [top height placed]} (:block-places app)
            :when (and (< top (+ scroll vh)) (> (+ top height) scroll))]
      (draw-boxes! app placed m (- m scroll) [0 m w vh]))
    (when (and caret? (not (command? app)))
      (if (insert? app) (draw-bar-caret! app) (draw-block-caret! app)))
    (sdl/set-render-clip-rect renderer ffi/null)
    (draw-scrollbar! app)
    (draw-status-bar! app)
    (when (and caret? (command? app))
      (draw-bar-caret! app))
    (draw-boxes! app (:float-places app) 0 0 nil)
    (sdl/render-present renderer)
    (textures/end-frame! textures)
    (textures/end-frame! (:ui-textures app))
    (assoc app :dirty? false :drawn-phase caret?)))
