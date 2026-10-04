(ns hoatzin.core
  "The desktop host: an SDL window whose events drive hoatzin.app."
  (:require [jolt.ffi :as ffi]
            [hoatzin.app :as app]
            [hoatzin.sdl :as sdl]))

;; ---------------------------------------------------------------- open dialog

;; SDL calls back once the user has chosen (or cancelled), from inside its
;; event loop, so from within a :blocking SDL_WaitEventTimeout or from
;; another thread: hence :collect-safe. The callback only records the choice
;; and wakes the event loop with a user event; `decode` reads the file.

(defn- open-dialog
  "{:show! f :take! g}: `show!` opens the dialog unless it is already open,
  and `take!` answers the choice it reported, once, as {:path p} or
  {:error e}, or nil."
  [window]
  (let [state (atom {:open? false})
        done  (fn [_ filelist _]
                (let [choice (cond (ffi/null? filelist) {:error (sdl/get-error)}
                                   (ffi/null? (ffi/read filelist :pointer)) nil
                                   :else {:path (ffi/ptr->string (ffi/read filelist :pointer))})]
                  (reset! state {:open? false :choice choice})
                  (ffi/with-alloc [ev sdl/EVENT-SIZE]
                    (ffi/write ev :uint sdl/EVENT-USER sdl/O-event-type)
                    (sdl/push-event ev))))
        cb (ffi/callback (ffi/global-arena) done [:pointer :pointer :int] :void :collect-safe)]
    {:show! (fn []
              (when-not (:open? @state)
                (swap! state assoc :open? true)
                (sdl/show-open-file-dialog cb ffi/null window ffi/null 0 ffi/null false)))
     :take! (fn []
              (let [{:keys [choice]} @state]
                (swap! state dissoc :choice)
                choice))}))

(defn- read-choice
  "The :opened event for a file chosen in the open dialog."
  [{:keys [path error]}]
  (if error
    {:type :opened :error error}
    (try {:type :opened :path path :text (slurp path)}
         (catch Exception e {:type :opened :path path :error (ex-message e)}))))

;; ---------------------------------------------------------------- events

(defn- decode
  "The SDL event in `ev` as a hoatzin.app event, or nil to ignore it.
  `dialog` is the open dialog, from `open-dialog`."
  [renderer dialog ev]
  (let [type (ffi/read ev :uint sdl/O-event-type)]
    (condp = type
      sdl/EVENT-QUIT       {:type :quit}
      sdl/EVENT-USER       (some-> ((:take! dialog)) read-choice)
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
       :dy (* (ffi/read ev :float sdl/O-wheel-y)
              (if (= sdl/MOUSEWHEEL-FLIPPED (ffi/read ev :uint sdl/O-wheel-direction)) -1 1))}
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
  [window latest dialog ev irect cursors]
  (let [renderer (:renderer @latest)
        step (fn [app]
               (if-let [e (decode renderer dialog ev)]
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
          dialog (open-dialog window)]
      (sdl/check! (sdl/start-text-input window) "SDL_StartTextInput")
      (try
        (reset! latest (app/create {:renderer     renderer
                                    :density-fn   #(sdl/get-window-pixel-density window)
                                    :clipboard-fn sdl/clipboard-text
                                    :set-clipboard-fn #(sdl/set-clipboard-text %)
                                    :open-dialog-fn (:show! dialog)
                                    :now          (sdl/get-ticks)}))
        (with-open [a (ffi/confined-arena)]
          (run-loop window latest dialog (ffi/alloc a sdl/EVENT-SIZE) (ffi/alloc a sdl/rect) cursors))
        (finally
          (some-> @latest app/destroy!)
          (run! sdl/destroy-cursor (vals cursors))
          (sdl/destroy-renderer renderer)
          (sdl/destroy-window window))))
    (finally
      (sdl/quit))))
