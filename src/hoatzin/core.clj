(ns hoatzin.core
  "The desktop host: an SDL window whose events drive hoatzin.app."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [jolt.ffi :as ffi]
            [hoatzin.app :as app]
            [hoatzin.app.journal :as journal]
            [hoatzin.app.recovery :as recovery]
            [hoatzin.lib.coretext :as ct]
            [hoatzin.lib.macos :as macos]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.app.settings :as settings]))

;; ---------------------------------------------------------------- file dialogs

;; SDL calls back once the user has chosen (or cancelled), from inside its
;; event loop, so from within a :blocking SDL_WaitEventTimeout or from
;; another thread: hence :collect-safe. The callback only records the choice
;; and wakes the event loop with a user event; `decode` reads the file.

(defn- file-dialogs
  "{:open! f :save! g :dir! h :take! i}: (`open!` dir) shows the open
  dialog, (`save!` path) the save dialog and (`dir!` dir) the folder
  dialog, each starting at the path it is given if it is not nil, unless a
  dialog is already showing. `take!` answers the choice reported, once, as
  {:dialog :open|:save|:dir} with :path p or :error e, or nil."
  [window]
  (let [state (atom {:showing nil})
        done  (fn [_ filelist _]
                (let [{:keys [showing location]} @state
                      choice (cond (ffi/null? filelist) {:error (sdl/get-error)}
                                   (ffi/null? (ffi/read filelist :pointer)) nil
                                   :else {:path (ffi/ptr->string (ffi/read filelist :pointer))})]
                  (some-> location ffi/free)
                  (reset! state {:showing nil :choice (some-> choice (assoc :dialog showing))})
                  (ffi/with-alloc [ev sdl/EVENT-SIZE]
                    (ffi/write ev :uint sdl/EVENT-USER sdl/O-event-type)
                    (sdl/push-event ev))))
        cb (ffi/callback (ffi/global-arena) done [:pointer :pointer :int] :void :collect-safe)
        ;; SDL may read the start location until it calls back, which frees it.
        show (fn [dialog path f]
               (when-not (:showing @state)
                 (let [location (some-> path ffi/string->ptr)]
                   (swap! state assoc :showing dialog :location location)
                   (f (or location ffi/null)))))]
    {:open! (fn [dir]
              (show :open dir #(sdl/show-open-file-dialog cb ffi/null window ffi/null 0 % false)))
     :save! (fn [path]
              (show :save path #(sdl/show-save-file-dialog cb ffi/null window ffi/null 0 %)))
     :dir!  (fn [dir]
              (show :dir dir #(sdl/show-open-folder-dialog cb ffi/null window % false)))
     :take! (fn []
              (let [{:keys [choice]} @state]
                (swap! state dissoc :choice)
                choice))}))

(defn- read-file
  "Read `path`: {:text s}, or {:error e}, why it could not."
  [path]
  (try {:text (slurp path)}
       (catch Exception e {:error (ex-message e)})))

(defn- read-choice
  "The event for a file chosen in a file dialog: for the open dialog,
  :opened with the file read."
  [{:keys [dialog path error]}]
  (cond
    (= dialog :save) (if error {:type :save-chosen :error error} {:type :save-chosen :path path})
    (= dialog :dir)  (if error {:type :dir-chosen :error error} {:type :dir-chosen :path path})
    error            {:type :opened :error error}
    :else            (merge {:type :opened :path path} (read-file path))))

(defn- user-modes
  "The user's own modes, from `dir`, as [origin source] pairs, and why
  any could not be read: {:sources [[origin source]] :errors [s]}."
  [dir]
  (reduce (fn [acc f]
            (try (update acc :sources conj [(str (fs/file-name f)) (slurp (str f))])
                 (catch Exception e
                   (update acc :errors conj (str "Can't read mode " f ": " (ex-message e))))))
          {:sources [] :errors []}
          (when (fs/directory? dir) (sort (fs/glob dir "*.clj")))))

(defn- write-file
  "Write string `s` to `path`: nil, or why it could not."
  [path s]
  (try (spit path s) nil
       (catch Exception e (ex-message e))))

;; ---------------------------------------------------------------- minor modes' data

;; A minor mode's data is read on a future's thread, so that the editor
;; never waits on it. The thread leaves what it read in `arrived`, an atom,
;; and wakes the event loop with a user event; the loop takes it from
;; there, so the app itself is only ever touched on the loop's thread.

(defn- mode-data-path
  "Where minor mode `mode` keeps its file `file`."
  [mode file]
  (str (fs/path (settings/modes-dir) mode file)))

(defn- wake!
  "Wake the event loop, from any thread."
  []
  (ffi/with-alloc [ev sdl/EVENT-SIZE]
    (ffi/write ev :uint sdl/EVENT-USER sdl/O-event-type)
    (sdl/push-event ev)))

(defn- mode-data
  "{:load! f :save! g :take! h}: (`load!` mode file) reads minor mode
  `mode`'s file `file` as EDN, on a thread of its own, and wakes the event
  loop; (`save!` mode file data) writes `data` there as EDN, or with
  `data` nil deletes it, returning nil, or why it could not; and `take!`
  answers what has been read since it last did, as :mode-data events."
  []
  (let [arrived (atom [])]
    {:load! (fn [mode file]
              (future
                (let [path (mode-data-path mode file)
                      read (try {:data (when (fs/exists? path) (edn/read-string (slurp path)))}
                                (catch Exception e {:error (ex-message e)}))]
                  (swap! arrived conj (merge {:type :mode-data :mode mode :file file} read))
                  (wake!)))
              nil)
     :save! (fn [mode file data]
              (let [path (mode-data-path mode file)]
                (try (if (nil? data)
                       (fs/delete-if-exists path)
                       (do (fs/create-dirs (fs/parent path))
                           (spit path (str (pr-str data) "\n"))))
                     nil
                     (catch Exception e (ex-message e)))))
     :take! (fn [] (first (reset-vals! arrived [])))}))

;; ---------------------------------------------------------------- events

(defn- decode
  "The SDL event in `ev` as a hoatzin.app event, or nil to ignore it.
  `dialogs` are the file dialogs, from `file-dialogs`."
  [renderer dialogs ev]
  (let [type (ffi/read ev :uint sdl/O-event-type)]
    (condp = type
      sdl/EVENT-QUIT       {:type :quit}
      sdl/EVENT-USER       (some-> ((:take! dialogs)) read-choice)
      sdl/EVENT-TEXT-INPUT {:type :text
                            :text (ffi/ptr->string (ffi/read ev :pointer sdl/O-text-text))}
      sdl/EVENT-TEXT-EDITING
      (let [start (ffi/read ev :int sdl/O-edit-start)]
        {:type   :composition
         :text   (ffi/ptr->string (ffi/read ev :pointer sdl/O-edit-text))
         :cursor (when-not (neg? start) start)})
      sdl/EVENT-KEY-DOWN   {:type :key
                            :key  (ffi/read ev :uint sdl/O-key-key)
                            :mod  (ffi/read ev :uint16 sdl/O-key-mod)}
      sdl/EVENT-MOUSE-BUTTON-DOWN
      (when (= sdl/BUTTON-LEFT (ffi/read ev :uint8 sdl/O-button-button))
        (sdl/convert-event-to-render-coordinates renderer ev)
        {:type :click
         :x (ffi/read ev :float sdl/O-button-x)
         :y (ffi/read ev :float sdl/O-button-y)
         :mod (sdl/get-mod-state)
         :clicks (ffi/read ev :uint8 sdl/O-button-clicks)})
      sdl/EVENT-MOUSE-BUTTON-UP
      (when (= sdl/BUTTON-LEFT (ffi/read ev :uint8 sdl/O-button-button))
        {:type :release})
      sdl/EVENT-MOUSE-MOTION
      (do (sdl/convert-event-to-render-coordinates renderer ev)
          {:type (if (pos? (bit-and sdl/BUTTON-LMASK (ffi/read ev :uint sdl/O-motion-state)))
                   :drag
                   :move)
           :x (ffi/read ev :float sdl/O-motion-x)
           :y (ffi/read ev :float sdl/O-motion-y)})
      sdl/EVENT-MOUSE-WHEEL
      {:type :wheel
       :dy (ffi/read ev :float sdl/O-wheel-y)}
      sdl/EVENT-WINDOW-FOCUS-GAINED {:type :focus :focused? true}
      sdl/EVENT-WINDOW-FOCUS-LOST   {:type :focus :focused? false}
      sdl/EVENT-WINDOW-EXPOSED      {:type :expose}
      sdl/EVENT-WINDOW-MOUSE-LEAVE  {:type :leave}
      nil)))

(defn- set-input-area!
  "Tell the input method where the caret is, in window points."
  [window app irect]
  (let [d (:density app)
        [x y w h] (app/caret-rect app)]
    (sdl/set-text-input-area window (sdl/set-rect! irect (/ x d) (/ y d) (max 1 (/ w d)) (/ h d)) 0)))

(defn- draw-now!
  "Draw the app in `latest`, settled, at its window's size now, if it needs
  it, and keep what drawing made of it."
  [window latest irect]
  (let [now (sdl/get-ticks)
        app (app/settle @latest now)
        app (if (app/needs-draw? app now)
              (let [app (app/draw! app now)]
                (set-input-area! window app irect)
                app)
              app)]
    (reset! latest app)))

;; While the window is dragged to a new size, macOS runs a loop of its own
;; inside SDL's, and ours waits: what it showed last would be stretched to
;; fit. SDL calls an event watch from that loop, on this thread, with an
;; expose event for each new size, and the app is drawn from there. So the
;; app is always the one in `latest`: the loop puts it there before each
;; wait and poll, in which the watch may draw it, and takes it back after.

(defn- live-resize-watch
  "An SDL event watch drawing the app in `latest` as the window is
  live-resized."
  [window latest irect]
  (ffi/callback (ffi/global-arena)
                (fn [_ e]
                  (when (and (= sdl/EVENT-WINDOW-EXPOSED (ffi/read e :uint sdl/O-event-type))
                             (= 1 (ffi/read e :int sdl/O-window-data1)))
                    ;; nothing may be thrown back through SDL
                    (try (reset! latest (app/handle @latest {:type :expose} (sdl/get-ticks)))
                         (draw-now! window latest irect)
                         (catch Exception e
                           (binding [*out* *err*] (println "Can't draw while resizing:" (ex-message e))))))
                  true)
                [:pointer :pointer] :bool :collect-safe))

(defn- run-loop
  "Run until quit. `latest` always holds the current app, for cleanup, and
  for drawing as the window is live-resized (see `live-resize-watch`).
  `cursors` maps hoatzin.app/pointer's answers to SDL cursors; `data` is
  the minor modes' data, from `mode-data`."
  [window latest dialogs data ev irect cursors]
  (let [renderer (:renderer @latest)
        step (fn [app]
               (let [user? (= sdl/EVENT-USER (ffi/read ev :uint sdl/O-event-type))
                     e     (decode renderer dialogs ev)
                     app   (if e (app/handle app e (sdl/get-ticks)) app)]
                 ;; a closed file dialog leaves the window without the focus
                 (when (and user? (#{:opened :save-chosen :dir-chosen} (:type e)))
                   (sdl/raise-window window))
                 ;; a user event may also be a minor mode's data, read
                 (if user?
                   (reduce #(app/handle %1 %2 (sdl/get-ticks)) app ((:take! data)))
                   app)))]
    (loop [shown-pointer nil]
      (when-not (:quit? @latest)
        (if (sdl/wait-event-timeout ev (app/ms-until-wake @latest (sdl/get-ticks)))
          ;; not swap!: a step has effects, which a retry would repeat
          (do (reset! latest (step @latest))
              (while (sdl/poll-event ev) (reset! latest (step @latest))))
          (reset! latest (app/handle @latest {:type :tick} (sdl/get-ticks))))
        (draw-now! window latest irect)
        (let [pointer (app/pointer @latest)]
          (when-not (= pointer shown-pointer)
            (sdl/set-cursor (cursors pointer)))
          (recur pointer))))))

(defn- recover
  "What a crashed session left: {:sessions [...] :message s}, :message why
  it could not be read, if it could not."
  [root]
  (try (let [sessions (recovery/sessions root)
             errors   (mapcat :errors sessions)]
         {:sessions sessions
          :message  (when (seq errors)
                      (str "Can't recover " (str/join ", " errors) " (kept in " root ")"))})
       (catch Exception e
         {:sessions [] :message (str "Can't look for a crashed session: " (ex-message e))})))

(defn -main [& _args]
  ;; We draw the composition (marked text) ourselves: SDL then sends it as
  ;; TEXT_EDITING events. The OS still draws the candidate list.
  (sdl/set-hint "SDL_IME_IMPLEMENTED_UI" "composition")
  (sdl/check! (sdl/init sdl/INIT-VIDEO) "SDL_Init")
  (try
    (let [[window renderer]
          (ffi/with-out [pwin :pointer]
            (ffi/with-out [pren :pointer]
              (sdl/check! (sdl/create-window-and-renderer
                           "Hoatzin" 800 600
                           (bit-or sdl/WINDOW-RESIZABLE sdl/WINDOW-HIGH-PIXEL-DENSITY)
                           pwin pren)
                          "SDL_CreateWindowAndRenderer")
              [(ffi/read pwin :pointer) (ffi/read pren :pointer)]))
          ;; cmd+m is the editor's (first non-blank), not Minimize's
          _       (try (macos/free-keys! "Minimize") (catch Exception _ nil))
          cursors {:text  (sdl/create-system-cursor sdl/SYSTEM-CURSOR-TEXT)
                   :arrow (sdl/create-system-cursor sdl/SYSTEM-CURSOR-DEFAULT)}
          latest (atom nil)
          dialogs (file-dialogs window)
          data    (mode-data)
          settings-file (settings/file)
          {:keys [error] :as loaded} (settings/read-file settings-file)
          modes (user-modes (settings/modes-dir))
          ;; what a crash left, and the log of this session's edits
          found   (recover (recovery/root))
          session (recovery/start! (recovery/root))]
      (sdl/check! (sdl/start-text-input window) "SDL_StartTextInput")
      (try
        (reset! latest (app/create {:renderer     renderer
                                    :density-fn   #(sdl/get-window-pixel-density window)
                                    :clipboard-fn sdl/clipboard-text
                                    :set-clipboard-fn #(sdl/set-clipboard-text %)
                                    :open-dialog-fn (:open! dialogs)
                                    :save-dialog-fn (:save! dialogs)
                                    :dir-dialog-fn (:dir! dialogs)
                                    :read-file-fn read-file
                                    :write-file-fn write-file
                                    :dir          (System/getProperty "user.dir")
                                    :settings     (:settings loaded)
                                    :save-settings-fn #(settings/write-file! settings-file %)
                                    :font-families-fn ct/font-families
                                    :load-mode-data-fn (:load! data)
                                    :save-mode-data-fn (:save! data)
                                    :mode-sources (:sources modes)
                                    :journal-fn   #(recovery/apply-ops! session %)
                                    :recovered    (mapcat :buffers (:sessions found))
                                    :message      (some->> (cond->> (:errors modes)
                                                             error (cons (str "Can't read settings: " error))
                                                             (:message found) (cons (:message found)))
                                                           seq
                                                           (str/join "; "))
                                    :now          (sdl/get-ticks)}))
        ;; a crashed session goes once this one has what it held
        (when-not (get-in @latest [:journal :error])
          (run! recovery/discard! (:sessions found)))
        (with-open [a (ffi/confined-arena)]
          (let [irect (ffi/alloc a sdl/rect)
                watch (live-resize-watch window latest irect)]
            (sdl/add-event-watch watch ffi/null)
            (try (run-loop window latest dialogs data (ffi/alloc a sdl/EVENT-SIZE) irect cursors)
                 (finally (sdl/remove-event-watch watch ffi/null)))))
        (finally
          ;; unsaved changes, if any, stay for the next start to recover:
          ;; the window may be closed, or `:quit!` used, with them
          (recovery/finish! session (boolean (some-> @latest journal/unsaved?)))
          (some-> @latest app/destroy!)
          (run! sdl/destroy-cursor (vals cursors))
          (sdl/destroy-renderer renderer)
          (sdl/destroy-window window))))
    (finally
      (sdl/quit)
      ;; Laying out a big text wraps it on futures' threads (see
      ;; hoatzin.lib.layout/paragraphs-of). Once idle, the pool keeps them
      ;; for a minute, and with them the process, unless it is shut down.
      (shutdown-agents))))
