(ns hoatzin.app
  "The editor as a state machine: plain-data events in, a new app out, and
  drawing the app with any SDL renderer.

  Nothing here reads the OS event queue, the clock, the clipboard or files:
  the host passes time into `handle`/`draw!` and supplies :density-fn,
  :clipboard-fn (read the clipboard), :set-clipboard-fn (write it),
  :open-dialog-fn (show a file dialog starting in the directory it is
  given, or nil; the file comes back as :opened), :save-dialog-fn (show a
  save dialog starting at the path it is given, or nil; the choice comes
  back as :save-chosen), :dir-dialog-fn (show a dialog choosing a
  directory, starting in the one it is given, or nil; the choice comes
  back as :dir-chosen), :read-file-fn (read path p, returning {:text s}, or
  {:error e}, why it could not), :write-file-fn (write string s to path p,
  returning nil, or why it could not),
  :save-settings-fn (persist the settings, as hoatzin.app.settings has them,
  returning nil, or why it could not), :font-families-fn (the names of
  the fonts installed, as hoatzin.lib.coretext/font-families gives them),
  :load-mode-data-fn (read what minor mode m keeps in file f, its data, as
  EDN, without waiting for it: it comes back as :mode-data, and it must be
  read where nothing waits on it) and :save-mode-data-fn (keep data d as
  minor mode m's file f, as EDN, or with d nil keep no such file,
  returning nil, or why it could not) and :journal-fn (keep the ops, a
  vector, of the write-ahead log of edits, as hoatzin.app.journal has them,
  returning nil, or why it could not). :rand-fn, a random number from 0
  to 1, is `rand` unless given.
  That is what lets tests drive the editor headlessly and deterministically.

  The editor is modal. In :normal mode the text is left alone: keys move the
  caret and select, `i` enters :insert mode, and the caret is a block. In
  :insert mode typing edits the text, and escape goes back to :normal.
  `:` starts a command line (:command mode) in the status bar; return runs
  it, escape abandons it, and tab completes the command's name. While it is
  open, a box above the status bar lists the commands that what is typed
  could still complete to. A command
  runs from any prefix that begins no other, so `:w` is `:write`.

  Each text is a buffer, after Emacs (see hoatzin.app.buffers): there is
  always the scratch buffer to begin with, each file opened is visited in
  a buffer of its own, and one buffer at a time is shown. Each has a
  directory, the working directory while it is shown. The file dialogs
  start where the last one chose, else in the working directory. Commands:
    :open                           choose a file and visit it, in a new
                                    buffer, or the one visiting it already
    :write                          save the buffer (choosing where, if it
                                    has no file yet)
    :save                           choose where to save the buffer, and save it
    :buffers                        list the buffers, over the text: return
                                    or a click switches to one
    :new                            start an empty buffer
    :close                          close the buffer, unless it has unsaved
                                    changes; `:close!` closes it regardless
    :revert                         read the buffer's file again
    :cd                             choose the buffer's directory
    :quit                           quit the editor, unless a buffer has
                                    unsaved changes; `:quit!` quits regardless
    :settings                       show the settings window, over the text
                                    until escape closes it

  Crash safety: after every event, what changed in the buffers with
  unsaved changes goes to :journal-fn, as a log of the edits (see
  hoatzin.app.journal), which the host keeps apart from the files the
  buffers visit: those are only written by :write. Given :recovered, the
  buffers a crashed session left (see hoatzin.app.recovery/sessions), the
  app starts with them, as they were, and says so.

  The scratch buffer's changes are its own: they don't stop a quit, or
  its closing, unless it has been saved to a file.

  Each buffer may be in a mode, after Emacs's major modes (see
  hoatzin.app.modes): Clojure evaluated by SCI, outside the editor, that
  says how its files are read and written and may add or replace commands
  and normal mode's keys, add its own to the help, ask yes or no in the
  status bar (see hoatzin.app.confirm), and add insets: boxes in the text
  holding text of their own, which the caret moves into and out of (see
  hoatzin.app.insets). A file opened or saved takes the mode for its
  extension, if there is one, and
    :mode name                      puts the buffer in mode `name`
                                    (\"text\" for none); with no name, says
                                    which it is in, and which there are
  Each buffer is in minor modes too, any number of them, which may add
  keys and commands as a mode does, and more; the editor's own is
  variants (see hoatzin.app.variants), on to begin with, and search (see
  hoatzin.app.search), which `/` turns on.
    :minor name                     turns minor mode `name` on in the
                                    buffer, or off; with no name, says
                                    which there are, and which are on

  While a window is open, it takes the input: the text is left alone.

  Clicking an editable field (see hoatzin.lib.ui) gives it the focus: then
  typing goes into it, backspace takes from its end, up and down step an
  integer field, tab and shift-tab move to the next and previous field or
  dropdown, and return or a click elsewhere gives the focus up. Clicking a
  dropdown gives it the focus and opens its list; with the focus, return,
  up, down or typing open it too. While the list is open it takes the
  input (see hoatzin.app.dropdown): choosing an option closes it, and the
  dropdown holds that option. In the settings window, a field or dropdown
  holding a valid setting changes it, which applies and saves it straight
  away; given up, a field shows the setting again.

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
    {:type :dir-chosen :path p}     the directory chosen, or
    {:type :dir-chosen :error e}    why the dialog failed
    {:type :mode-data :mode m :file f :data d}
                                    minor mode m's file f, read as
                                    :load-mode-data-fn asked (d nil if there
                                    is no such file), or
    {:type :mode-data :mode m :file f :error e}
                                    why it could not be
    {:type :expose}                 the window needs repainting
    {:type :quit}

  This namespace is the app's interface, and routes each event to where
  it is handled. The rest is in hoatzin.app.*; the app map itself is
  described in hoatzin.app.state."
  (:require [clojure.string :as str]
            [jolt.ffi :as ffi]
            [hoatzin.app.boxes :as boxes]
            [hoatzin.app.buffers :as buffers]
            [hoatzin.app.caret :as caret]
            [hoatzin.app.command :as command]
            [hoatzin.app.choose :as choose]
            [hoatzin.app.confirm :as confirm]
            [hoatzin.app.draw :as draw]
            [hoatzin.app.dropdown :as dropdown]
            [hoatzin.app.files :as files]
            [hoatzin.app.input.fields :as fields]
            [hoatzin.app.input.keyboard :as keyboard]
            [hoatzin.app.input.mouse :as mouse]
            [hoatzin.app.insets :as insets]
            [hoatzin.app.journal :as journal]
            [hoatzin.app.xsel :as xsel]
            [hoatzin.app.modes :as modes]
            [hoatzin.app.rename :as rename]
            [hoatzin.app.scroll :as scroll]
            [hoatzin.app.settings :as settings]
            [hoatzin.app.state :as state]
            [hoatzin.app.sync :as sync]
            [hoatzin.app.search :as search]
            [hoatzin.app.variants :as variants]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.textures :as textures]
            [hoatzin.lib.ui :as ui]))

;; ---------------------------------------------------------------- lifecycle

(defn- recovered-message [recovered]
  (str "Recovered " (count recovered) " unsaved buffer" (when (> (count recovered) 1) "s") ": "
       (str/join ", " (map #(or (some-> (get-in % [:meta :path]) files/file-name)
                                (get-in % [:meta :name]))
                           recovered))
       " (their files are as last saved)"))

(defn create
  "A new, empty editor drawing with `:renderer`. Options (all but :renderer
  optional): :density-fn, :clipboard-fn, :set-clipboard-fn, :open-dialog-fn,
  :save-dialog-fn, :dir-dialog-fn, :read-file-fn, :write-file-fn,
  :save-settings-fn, :font-families-fn, :load-mode-data-fn,
  :save-mode-data-fn, :journal-fn, :recovered (buffers to start with,
  after a crash), :now, :dir (the working
  directory, if any), :settings (the defaults unless given), :mode
  (:normal unless given), :mode-sources (modes besides the editor's own,
  as [origin source] pairs: see hoatzin.app.modes/load-sources), :message,
  and any key of hoatzin.app.state/defaults. It has one buffer, the
  scratch buffer. A mode that can't be loaded is left out, and the
  message says why. Release it with `destroy!`."
  [{:keys [now dir mode-sources message recovered] :or {now 0} :as opts}]
  (let [{:keys [modes errors]} (modes/load-sources (concat (modes/builtin-sources) mode-sources))
        app (sync/settle
             (buffers/init
              (merge state/defaults
                     {:density-fn   (constantly 1.0)
                      :clipboard-fn (constantly "")
                      :set-clipboard-fn (fn [_])
                      :open-dialog-fn (fn [_])
                      :save-dialog-fn (fn [_])
                      :dir-dialog-fn (fn [_])
                      :read-file-fn (fn [_] {:error "no file system"})
                      :write-file-fn (fn [_ _] "no file system")
                      :save-settings-fn (fn [_])
                      :font-families-fn (constantly [])
                      :rand-fn      rand
                      :load-mode-data-fn (fn [_ _])
                      :save-mode-data-fn (fn [_ _ _])
                      :journal-fn   (fn [_])
                      :option-faces (atom {})
                      :settings     settings/defaults
                      :textures     (textures/cache)
                      :ui-textures  (textures/cache)
                      :floats       []
                      :ui-values    {}
                      :scratch      {:frect (ffi/alloc sdl/frect) :irect (ffi/alloc sdl/rect)}
                      :mode         :normal
                      :focused?     true
                      :blink-from   now
                      :active-at    now
                      :dirty?       true}
                     (dissoc opts :now :dir :mode-sources :recovered)
                     {:modes   (merge {(:name variants/mode) variants/mode (:name search/mode) search/mode} modes)
                      :message (some->> (seq (remove nil? (cons message errors)))
                                        (str/join "; "))})
              dir))
        restored (if (seq recovered)
                   (-> (buffers/restore app now recovered)
                       (update :message #(str/join "; " (remove nil? [(recovered-message recovered) %]))))
                   app)]
    ;; what was recovered is kept again, as the first thing
    (journal/observe restored now)))

(defn destroy! [app]
  (sync/release-view! app)
  (run! ffi/free (vals (:scratch app))))

;; ---------------------------------------------------------------- events

(defn- normal? [app] (= :normal (:mode app)))

(defn- on-text-event
  "An event for the text: no field has the focus, and no window is open
  or it left the event alone. What edits the text edits the inset the
  caret is in, if it is in one (see hoatzin.app.insets/in-view); the
  mode's bindings and the command line work on the buffer."
  [app now event]
  (let [composing? (insets/composing? app)
        in-text    (fn [f] (insets/in-view app f))
        click      #(mouse/on-click % now (:x event) (:y event) (:mod event 0) (:clicks event 1))]
    (case (:type event)
      :quit   (assoc app :quit? true)
      ;; Over a folded section there is no text to edit: only the
      ;; commands that leave it alone work.
      :text   (let [typed (fn [app]
                            (or (when (normal? app)
                                  (some-> (modes/normal-command app (:text event)) (as-> f (f app now))))
                                (if (insets/over app)
                                  (if (and (normal? app) (#{":" "?"} (:text event)))
                                    (keyboard/on-text app now (:text event))
                                    app)
                                  (insets/in-view app #(keyboard/on-text % now (:text event))))))]
                (cond (state/command? app) (keyboard/on-text app now (:text event))
                      :else (or (xsel/on-text app now (:text event) typed) (typed app))))
      ;; Normal mode has no use for the input method's marked text.
      :composition (let [app (if (and (:xsel app) (state/insert? app) (seq (:text event)))
                               (xsel/delete-selection app now)
                               app)]
                     (if (and (state/insert? app) (not (insets/over app)))
                       (in-text #(keyboard/compose % now (:text event) (:cursor event)))
                       app))
      ;; While composing, keys and clicks belong to the input method, and the
      ;; layout shows the composition, so its positions aren't the document's.
      :key    (let [{:keys [key mod] :or {mod 0}} event]
                (let [pressed (fn [app]
                                (or (when (and (state/insert? app) (= key sdl/K-BACKSPACE)
                                               (zero? (bit-and mod (bit-or sdl/KMOD-GUI sdl/KMOD-CTRL sdl/KMOD-ALT))))
                                      (insets/backspace-into app now))
                                    (some-> (cond (normal? app)       (modes/normal-key app key mod)
                                                  (state/insert? app) (modes/insert-key app key mod))
                                            (as-> f (f app now)))
                                    (insets/cross app now key mod)
                                    (if (insets/over app)
                                      (if (= sdl/K-ESCAPE key) (state/enter-mode app now :normal) app)
                                      (insets/in-view app #(keyboard/on-key % now key mod)))))]
                  (cond composing? app
                        (state/command? app) (command/on-key app now key)
                        :else (or (xsel/on-key app now key mod pressed) (pressed app)))))
      ;; The scroll bar leaves the document alone, so it works while composing.
      ;; Boxes take the clicks on them, over the text and the scroll bar;
      ;; a click anywhere else gives up the focus. A click in an inset puts
      ;; the caret there, and one on its header folds or unfolds it.
      :click  (let [{:keys [x y]} event
                    app   (-> app (dissoc :selecting? :xdrag) xsel/clear)
                    inset (when-not (ui/hit (:float-places app) x y) (insets/hit app x y))
                    hit   (boxes/ui-hit app x y)]
                (cond
                  inset (let [app (fields/blur app)]
                          (if (and composing? (= :body (:part inset)))
                            app
                            (insets/click app now x y click)))
                  hit   (fields/on-ui-click app now hit)
                  :else (let [app (fields/blur app)]
                          (cond (scroll/on-scrollbar? app x) (scroll/on-scrollbar-click app y)
                                composing? app
                                :else (click (insets/leave app now))))))
      :drag   (cond (:grab app) (scroll/on-thumb-drag app (:y event))
                    composing? app
                    :else (or (xsel/drag app now (:x event) (:y event))
                              (in-text #(mouse/on-drag % now (:x event) (:y event)))))
      :release (-> (mouse/on-release app) (dissoc :xdrag))
      :move   (-> (scroll/hover app (boolean (scroll/on-scrollbar? app (:x event))))
                  (boxes/hover (:x event) (:y event))
                  (assoc :pointer [(:x event) (:y event)]))
      :leave  (-> (scroll/hover app false) (boxes/hover nil nil) (dissoc :pointer))
      ;; a drag held outside the window scrolls the buffer's text, not an inset's
      :tick   (if (:inset app) app (mouse/autoscroll app now))
      ;; the wheel over an inset that scrolls scrolls it
      :wheel  (let [[x y] (:pointer app)]
                (or (insets/on-wheel app x y (:dy event))
                    (scroll/on-wheel app now (:dy event))))
      :focus  (assoc app :focused? (:focused? event) :blink-from now :dirty? true)
      :opened (buffers/open-file app now event)
      :save-chosen (files/save-chosen app event)
      :dir-chosen (buffers/dir-chosen app event)
      :expose (assoc app :dirty? true)
      app)))

(defn- handle-event
  "The app after `event` at time `now`, but for the log of edits."
  [app event now]
  (let [;; once the editor font's wait is over, sync-view makes it
        app (if (some-> (:fonts-at app) (<= now)) (dissoc app :fonts-at) app)
        app (sync/sync-view app now)        ; navigation needs a fresh layout
        ;; a message lasts until the next keystroke
        app (if (and (:message app) (#{:key :text} (:type event)))
              (-> app (dissoc :message) (assoc :dirty? true))
              app)
        ;; a field that is gone keeps no focus, and a dropdown no list
        app (if (and (:focus app) (nil? (boxes/focused-field app))) (fields/blur app) app)
        app (if (and (:list app) (nil? (dropdown/place app))) (dropdown/close app) app)
        ;; and a list closes as the window loses the focus
        app (if (and (= :focus (:type event)) (not (:focused? event))) (dropdown/close app) app)
        app (rename/keep-for app event)
        app (if (#{:tick :expose} (:type event)) app (assoc app :active-at now))
        app (dropdown/glide app now)
        app (scroll/glide app now)]
    (or
     (when (= :mode-data (:type event)) (modes/loaded app event))
     (when (:confirm app) (confirm/on-event app now event))
     (when (:choose app) (choose/on-event app now event))
     (when (:renaming app) (rename/on-event app event))
     (modes/on-event app now event)
     (when (:list app) (dropdown/on-event app now event))
     (when (:focus app) (fields/on-focus-event app now event))
     (when (:window app) (fields/on-window-event app now event))
     (on-text-event app now event))))

(defn handle
  "The app after `event` (see the ns doc) at time `now` (ms), and the log
  of what it changed (see hoatzin.app.journal). A minor
  mode's data goes to that mode, whatever else is happening. A question
  in the status bar, if any, takes the event first (see
  hoatzin.app.confirm), then the section being renamed, then a minor
  mode of the buffer's, then an open dropdown list, then the field or
  dropdown with the focus, then the open window, then the text."
  [app event now]
  (journal/observe (handle-event app event now) now))

;; ---------------------------------------------------------------- the host's loop

(defn settle
  "Sync the view, then scroll as the last batch of events asked. Given
  the time, `now` (ms), a long text waits for a new width to hold before
  it wraps to it (see hoatzin.app.sync/sync-size)."
  ([app] (sync/settle app))
  ([app now] (sync/settle app now)))

(defn ms-until-wake
  "How long the host may sleep before sending a :tick: until the caret next
  toggles, a held drag next scrolls, the editor font is to be applied, a
  long text is to wrap to a new width, or a gliding dropdown list next
  moves, or the insets' changes are to be kept for recovery, or
  indefinitely (-1) when nothing changes without an event."
  [app now]
  (let [b (:blink-ms app)
        waits (cond-> []
                (and (caret/caret-blinking? app) (not (caret/idle? app now)))
                (conj (min (- b (mod (- now (:blink-from app)) b))
                           (- (+ (:active-at app) (:blink-idle-ms app)) now)))
                (mouse/autoscrolling? app)  (conj (:autoscroll-ms app))
                (:fonts-at app)             (conj (max 0 (- (:fonts-at app) now)))
                (:wrap-at app)              (conj (max 0 (- (:wrap-at app) now)))
                (dropdown/gliding? app)     (conj (:frame-ms app))
                (scroll/gliding? app)       (conj (:frame-ms app))
                (journal/due-at app)        (conj (max 0 (- (journal/due-at app) now))))]
    (if (seq waits) (reduce min waits) -1)))

(defn pointer
  "The mouse cursor the pointer should show: :arrow over the scroll bar
  or a box, or while a window or a dropdown's list is open, else :text."
  [app]
  (if (or (:hover? app) (:grab app) (:ui-hover? app) (:window app) (:list app)) :arrow :text))

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
