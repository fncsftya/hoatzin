(ns hoatzin.core
  "The desktop host: an SDL window whose events drive hoatzin.app."
  (:require [jolt.ffi :as ffi]
            [hoatzin.app :as app]
            [hoatzin.sdl :as sdl]))

(defn- decode
  "The SDL event in `ev` as a hoatzin.app event, or nil to ignore it."
  [renderer ev]
  (let [type (ffi/read ev :uint sdl/O-event-type)]
    (condp = type
      sdl/EVENT-QUIT       {:type :quit}
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
      (when (pos? (bit-and sdl/BUTTON-LMASK (ffi/read ev :uint sdl/O-motion-state)))
        (sdl/convert-event-to-render-coordinates renderer ev)
        {:type :drag
         :x (ffi/read ev :float sdl/O-motion-x)
         :y (ffi/read ev :float sdl/O-motion-y)})
      sdl/EVENT-MOUSE-WHEEL
      {:type :wheel
       :dy (* (ffi/read ev :float sdl/O-wheel-y)
              (if (= sdl/MOUSEWHEEL-FLIPPED (ffi/read ev :uint sdl/O-wheel-direction)) -1 1))}
      sdl/EVENT-WINDOW-FOCUS-GAINED {:type :focus :focused? true}
      sdl/EVENT-WINDOW-FOCUS-LOST   {:type :focus :focused? false}
      sdl/EVENT-WINDOW-EXPOSED      {:type :expose}
      nil)))

(defn- set-input-area!
  "Tell the input method where the caret is, in window points."
  [window app irect]
  (let [d (:density app)
        [x y w h] (app/caret-rect app)]
    (sdl/set-text-input-area window (sdl/set-rect! irect (/ x d) (/ y d) (max 1 (/ w d)) (/ h d)) 0)))

(defn- run-loop
  "Run until quit. `latest` always holds the current app, for cleanup."
  [window latest ev irect]
  (let [renderer (:renderer @latest)
        step (fn [app]
               (if-let [e (decode renderer ev)]
                 (app/handle app e (sdl/get-ticks))
                 app))]
    (loop [app @latest]
      (reset! latest app)
      (when-not (:quit? app)
        (let [app (if (sdl/wait-event-timeout ev (app/ms-until-blink app (sdl/get-ticks)))
                    (loop [app (step app)]
                      (if (sdl/poll-event ev) (recur (step app)) app))
                    app)
              app (app/settle app)
              now (sdl/get-ticks)]
          (if (app/needs-draw? app now)
            (let [app (app/draw! app now)]
              (set-input-area! window app irect)
              (recur app))
            (recur app)))))))

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
          cursor (sdl/create-system-cursor sdl/SYSTEM-CURSOR-TEXT)
          latest (atom nil)]
      (sdl/set-cursor cursor)
      (sdl/check! (sdl/start-text-input window) "SDL_StartTextInput")
      (try
        (reset! latest (app/create {:renderer     renderer
                                    :density-fn   #(sdl/get-window-pixel-density window)
                                    :clipboard-fn sdl/clipboard-text
                                    :set-clipboard-fn #(sdl/set-clipboard-text %)
                                    :now          (sdl/get-ticks)}))
        (with-open [a (ffi/confined-arena)]
          (run-loop window latest (ffi/alloc a sdl/EVENT-SIZE) (ffi/alloc a sdl/rect)))
        (finally
          (some-> @latest app/destroy!)
          (sdl/destroy-cursor cursor)
          (sdl/destroy-renderer renderer)
          (sdl/destroy-window window))))
    (finally
      (sdl/quit))))
