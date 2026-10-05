(ns hoatzin.core
  "The desktop host: an SDL window whose events drive hoatzin.app."
  (:require [jolt.ffi :as ffi]
            [hoatzin.app :as app]
            [hoatzin.lib.coretext :as ct]
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

(defn- write-file
  "Write string `s` to `path`: nil, or why it could not."
  [path s]
  (try (spit path s) nil
       (catch Exception e (ex-message e))))

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

(defn- run-loop
  "Run until quit. `latest` always holds the current app, for cleanup.
  `cursors` maps hoatzin.app/pointer's answers to SDL cursors."
  [window latest dialogs ev irect cursors]
  (let [renderer (:renderer @latest)
        step (fn [app]
               ;; a closed file dialog leaves the window without the focus
               (when (= sdl/EVENT-USER (ffi/read ev :uint sdl/O-event-type))
                 (sdl/raise-window window))
               (if-let [e (decode renderer dialogs ev)]
                 (app/handle app e (sdl/get-ticks))
                 app))]
    (loop [app @latest, shown-pointer nil]
      (reset! latest app)
      (when-not (:quit? app)
        (let [app (if (sdl/wait-event-timeout ev (app/ms-until-wake app (sdl/get-ticks)))
                    (loop [app (step app)]
                      (if (sdl/poll-event ev) (recur (step app)) app))
                    (app/handle app {:type :tick} (sdl/get-ticks)))
              app (app/settle app)
              now (sdl/get-ticks)
              pointer (app/pointer app)]
          (when-not (= pointer shown-pointer)
            (sdl/set-cursor (cursors pointer)))
          (if (app/needs-draw? app now)
            (let [app (app/draw! app now)]
              (set-input-area! window app irect)
              (recur app pointer))
            (recur app pointer)))))))

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
          cursors {:text  (sdl/create-system-cursor sdl/SYSTEM-CURSOR-TEXT)
                   :arrow (sdl/create-system-cursor sdl/SYSTEM-CURSOR-DEFAULT)}
          latest (atom nil)
          dialogs (file-dialogs window)
          settings-file (settings/file)
          {:keys [error] :as loaded} (settings/read-file settings-file)]
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
                                    :message      (when error (str "Can't read settings: " error))
                                    :now          (sdl/get-ticks)}))
        (with-open [a (ffi/confined-arena)]
          (run-loop window latest dialogs (ffi/alloc a sdl/EVENT-SIZE) (ffi/alloc a sdl/rect) cursors))
        (finally
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
