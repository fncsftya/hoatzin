(ns hoatzin.app
  "The editor as a state machine: plain-data events in, a new app out, and
  drawing the app with any SDL renderer.

  Nothing here reads the OS event queue, the clock, the clipboard or files:
  the host passes time into `handle`/`draw!` and supplies :density-fn,
  :clipboard-fn (read the clipboard), :set-clipboard-fn (write it),
  :open-dialog-fn (show a file dialog, whose file comes back as :opened),
  :save-dialog-fn (show a save dialog starting at the path it is given, or
  nil; the choice comes back as :save-chosen), :write-file-fn (write
  string s to path p, returning nil, or why it could not) and
  :save-settings-fn (persist the settings, as hoatzin.app.settings has them,
  returning nil, or why it could not).
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

  Clicking an editable field (see hoatzin.lib.ui) gives it the focus: then
  typing goes into it, backspace takes from its end, up and down step an
  integer field, tab and shift-tab move to the next and previous field,
  and return or a click elsewhere gives the focus up. In the settings
  window, a field holding a valid setting changes it, which applies and
  saves it straight away; given up, it shows the setting again.

  Events:
    {:type :text  :text s}          committed text input; in normal mode, a
                                    command
    {:type :composition :text s :cursor i}
                                    input-method composition (marked text),
                                    e.g. the accent after option-e; \"\" ends it.
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
    {:type :quit}

  This namespace is the app's interface, and routes each event to where
  it is handled. The rest is in hoatzin.app.*; the app map itself is
  described in hoatzin.app.state."
  (:require [jolt.ffi :as ffi]
            [hoatzin.app.boxes :as boxes]
            [hoatzin.app.caret :as caret]
            [hoatzin.app.command :as command]
            [hoatzin.app.draw :as draw]
            [hoatzin.app.files :as files]
            [hoatzin.app.input.fields :as fields]
            [hoatzin.app.input.keyboard :as keyboard]
            [hoatzin.app.input.mouse :as mouse]
            [hoatzin.app.scroll :as scroll]
            [hoatzin.app.settings :as settings]
            [hoatzin.app.state :as state]
            [hoatzin.app.sync :as sync]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.textures :as textures]))

;; ---------------------------------------------------------------- lifecycle

(defn create
  "A new, empty editor drawing with `:renderer`. Options (all but :renderer
  optional): :density-fn, :clipboard-fn, :set-clipboard-fn, :open-dialog-fn,
  :save-dialog-fn, :write-file-fn, :save-settings-fn, :now, :settings (the
  defaults unless given), :mode (:normal unless given), :message, and any
  key of hoatzin.app.state/defaults.
  Release it with `destroy!`."
  [{:keys [now] :or {now 0} :as opts}]
  (sync/settle (merge state/defaults
                      {:density-fn   (constantly 1.0)
                       :clipboard-fn (constantly "")
                       :set-clipboard-fn (fn [_])
                       :open-dialog-fn (fn [])
                       :save-dialog-fn (fn [_])
                       :write-file-fn (fn [_ _] "no file system")
                       :save-settings-fn (fn [_])
                       :settings     settings/defaults
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
  (sync/release-view! app)
  (run! ffi/free (vals (:scratch app))))

;; ---------------------------------------------------------------- events

(defn- on-text-event
  "An event for the text: no field has the focus, and no window is open
  or it left the event alone."
  [app now event]
  (let [composing? (some? (:composition app))]
    (case (:type event)
      :quit   (assoc app :quit? true)
      :text   (keyboard/on-text app now (:text event))
      ;; Normal mode has no use for the input method's marked text.
      :composition (if (state/insert? app)
                     (keyboard/compose app now (:text event) (:cursor event))
                     app)
      ;; While composing, keys and clicks belong to the input method, and the
      ;; layout shows the composition, so its positions aren't the document's.
      :key    (cond composing? app
                    (state/command? app) (command/on-key app now (:key event))
                    :else (keyboard/on-key app now (:key event) (:mod event 0)))
      ;; The scroll bar leaves the document alone, so it works while composing.
      ;; Boxes take the clicks on them, over the text and the scroll bar;
      ;; a click anywhere else gives up the focus.
      :click  (if-let [hit (fields/ui-hit app (:x event) (:y event))]
                (fields/on-ui-click app now hit)
                (let [app (fields/blur app)]
                  (cond (scroll/on-scrollbar? app (:x event))
                        (scroll/on-scrollbar-click app (:y event))
                        composing? app
                        :else (mouse/on-click app now (:x event) (:y event)
                                              (:mod event 0) (:clicks event 1)))))
      :drag   (cond (:grab app) (scroll/on-thumb-drag app (:y event))
                    composing? app
                    :else (mouse/on-drag app now (:x event) (:y event)))
      :release (mouse/on-release app)
      :move   (-> (scroll/hover app (boolean (scroll/on-scrollbar? app (:x event))))
                  (assoc :ui-hover? (some? (fields/ui-hit app (:x event) (:y event)))))
      :leave  (-> (scroll/hover app false) (dissoc :ui-hover?))
      :tick   (mouse/autoscroll app now)
      :wheel  (scroll/on-wheel app (:dy event))
      :focus  (assoc app :focused? (:focused? event) :blink-from now :dirty? true)
      :opened (files/load-file app now event)
      :save-chosen (files/save-chosen app event)
      :expose (assoc app :dirty? true)
      app)))

(defn handle
  "The app after `event` (see the ns doc) at time `now` (ms). The field
  with the focus, if any, takes the event first, then the open window, if
  any, then the text."
  [app event now]
  (let [;; once the editor font's wait is over, sync-view makes it
        app (if (some-> (:fonts-at app) (<= now)) (dissoc app :fonts-at) app)
        app (sync/sync-view app)            ; navigation needs a fresh layout
        ;; a message lasts until the next keystroke
        app (if (and (:message app) (#{:key :text} (:type event)))
              (-> app (dissoc :message) (assoc :dirty? true))
              app)
        ;; a field that is gone keeps no focus
        app (if (and (:focus app) (nil? (fields/focused-field app))) (fields/blur app) app)]
    (or
      (when (:focus app) (fields/on-focus-event app now event))
      (when (:window app) (fields/on-window-event app now event))
      (on-text-event app now event))))

;; ---------------------------------------------------------------- the host's loop

(defn settle
  "Sync the view, then scroll as the last batch of events asked."
  [app]
  (sync/settle app))

(defn ms-until-wake
  "How long the host may sleep before sending a :tick: until the caret next
  toggles, a held drag next scrolls or the editor font is to be applied, or
  indefinitely (-1) when nothing changes without an event."
  [app now]
  (let [b (:blink-ms app)
        waits (cond-> []
                (caret/caret-blinking? app) (conj (- b (mod (- now (:blink-from app)) b)))
                (mouse/autoscrolling? app)  (conj (:autoscroll-ms app))
                (:fonts-at app)             (conj (max 0 (- (:fonts-at app) now))))]
    (if (seq waits) (reduce min waits) -1)))

(defn pointer
  "The mouse cursor the pointer should show: :arrow over the scroll bar
  or a box, or while a window is open, else :text."
  [app]
  (if (or (:hover? app) (:grab app) (:ui-hover? app) (:window app)) :arrow :text))

(defn needs-draw? [app now]
  (or (:dirty? app) (not= (caret/caret-visible? app now) (:drawn-phase app))))

(defn draw!
  "Render the app at time `now` (ms), present it, and return the app."
  [app now]
  (draw/draw! app now))

(defn caret-rect
  "The caret's [x y w h] in render pixels: in the field with the focus, on
  the command line, or in the text."
  [app]
  (caret/caret-rect app))

(defn caret-visible?
  "Whether the caret is shown at time `now` (ms)."
  [app now]
  (caret/caret-visible? app now))

(defn scrollbar
  "The scroll bar in render pixels, or nil when the text fits: see
  hoatzin.app.scroll/scrollbar."
  [app]
  (scroll/scrollbar app))

;; ---------------------------------------------------------------- boxes

(defn add-block
  "Show box `node` (see hoatzin.lib.ui) in the text, below the paragraph
  holding position `pos` and as wide as the text, which makes room for it.
  It keeps to that paragraph as the text is edited. Replaces any block
  `id` was."
  [app id pos node]
  (boxes/add-block app id pos node))

(defn remove-block [app id] (boxes/remove-block app id))

(defn set-floats
  "Show boxes `nodes` above everything, placed in the window: those in
  flow down its left edge, absolute ones where they say. Later ones are
  drawn over earlier ones."
  [app nodes]
  (boxes/set-floats app nodes))

(defn set-ui-value
  "Set what the interactive box `id` holds."
  [app id value]
  (boxes/set-ui-value app id value))
