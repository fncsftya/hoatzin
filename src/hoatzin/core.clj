(ns hoatzin.core
  "A minimal text editor: SDL3 for the window and input, CoreText for text."
  (:require [clojure.string :as str]
            [jolt.ffi :as ffi]
            [hoatzin.coretext :as ct]
            [hoatzin.editor :as ed]
            [hoatzin.layout :as layout]
            [hoatzin.sdl :as sdl]
            [hoatzin.textures :as textures]))

(def font-size 20)   ; points; scaled by the window's pixel density
(def margin 24)      ; points
(def blink-ms 530)   ; the macOS caret blink period
(def wheel-lines 3)

(def background [30 30 46])
(def foreground [235 230 220])

;; The app is a map threaded through the loop:
;;   :window :renderer :cursor  SDL handles
;;   :density :font             the font, at the window's current pixel density
;;   :ctx :layout :laid-out     layout context, current layout, the text it is of
;;   :textures                  line texture cache
;;   :doc :goal-x               the document; the column vertical moves aim for
;;   :size :scroll              output size and vertical scroll, in pixels
;;   :focused? :blink-from      caret blinking restarts from :blink-from
;;   :dirty? :drawn-phase       redraw needed / caret phase last drawn
;;   :follow?                   scroll the caret into view after this batch

(defn- px [app pts] (long (Math/round (* (double pts) (:density app)))))

;; ---------------------------------------------------------------- view

(defn- release-view! [{:keys [ctx textures font]}]
  (some-> ctx layout/release-context)
  (some-> textures textures/clear!)
  (some-> font ct/release-font))

(defn- sync-view
  "Bring font, layout context and layout up to date with the window and text.
  Each step is a cheap comparison unless its inputs changed."
  [app]
  (let [density (double (sdl/get-window-pixel-density (:window app)))
        app (if (= density (:density app))
              app
              (do (release-view! app)
                  (assoc app :density density
                             :font (ct/font (* font-size density))
                             :ctx nil)))
        [w _ :as size] (sdl/render-output-size (:renderer app))
        wrap (max 1 (- w (* 2 (px app margin))))
        app (if (= wrap (get-in app [:ctx :width]))
              app
              (do (some-> (:ctx app) layout/release-context)
                  ;; Re-wrapping moves every line; keep the caret's in view.
                  (assoc app :ctx (layout/context (:font app) wrap) :layout nil :follow? true)))
        text (get-in app [:doc :text])
        app (if (= size (:size app)) app (assoc app :size size :dirty? true))]
    (if (and (:layout app) (identical? text (:laid-out app)))
      app
      (assoc app :layout (layout/layout (:ctx app) text) :laid-out text :dirty? true))))

(defn- view-height [app] (- (second (:size app)) (* 2 (px app margin))))

(defn- clamp-scroll [app]
  (let [L (:layout app)
        content (* (layout/line-count L) (layout/line-height L))]
    (update app :scroll #(max 0 (min % (- content (view-height app)))))))

(defn- follow-caret
  "Scroll just enough to bring the caret's line into view."
  [app]
  (let [L  (:layout app)
        lh (layout/line-height L)
        [_ k] (layout/caret L (get-in app [:doc :caret]))
        top (* k lh)
        vh (view-height app)
        s  (:scroll app)]
    (assoc app :scroll (cond (< top s) top
                             (> (+ top lh) (+ s vh)) (- (+ top lh) vh)
                             :else s))))

;; ---------------------------------------------------------------- input

(defn- touched
  "After an edit or caret move: show the caret solid and keep it in view."
  [app]
  (assoc app :follow? true :dirty? true :blink-from (sdl/get-ticks)))

(defn- edit [app f & args]
  (-> app (assoc :doc (apply f (:doc app) args) :goal-x nil) touched))

(defn- move-to
  ([app pos] (move-to app pos nil))
  ([app pos goal-x]
   (-> app (assoc :doc (ed/move (:doc app) pos) :goal-x goal-x) touched)))

(defn- move-lines
  "Move the caret `n` visual lines, aiming for the remembered column."
  [app n]
  (let [L (:layout app)
        [x k] (layout/caret L (get-in app [:doc :caret]))
        goal (or (:goal-x app) x)
        k2 (+ k n)]
    (cond
      (neg? k2) (move-to app 0)
      (>= k2 (layout/line-count L)) (move-to app (count (get-in app [:doc :text])))
      :else (move-to app (layout/position-at L k2 goal) goal))))

(defn- on-key [app key mod]
  (let [L    (:layout app)
        pos  (get-in app [:doc :caret])
        cmd? (pos? (bit-and mod sdl/KMOD-GUI))
        [_ k] (layout/caret L pos)
        page (max 1 (quot (view-height app) (layout/line-height L)))]
    (condp = key
      sdl/K-BACKSPACE (cond cmd?        (edit app ed/delete (layout/line-start L k) pos)
                            (pos? pos)  (edit app ed/delete (layout/prev-position L pos) pos)
                            :else       app)
      sdl/K-DELETE    (edit app ed/delete pos (layout/next-position L pos))
      sdl/K-RETURN    (edit app ed/insert "\n")
      sdl/K-KP-ENTER  (edit app ed/insert "\n")
      sdl/K-LEFT      (move-to app (if cmd? (layout/line-start L k) (layout/prev-position L pos)))
      sdl/K-RIGHT     (move-to app (if cmd? (layout/line-end L k) (layout/next-position L pos)))
      sdl/K-HOME      (move-to app (layout/line-start L k))
      sdl/K-END       (move-to app (layout/line-end L k))
      sdl/K-UP        (if cmd? (move-to app 0) (move-lines app -1))
      sdl/K-DOWN      (if cmd? (move-to app (count (get-in app [:doc :text]))) (move-lines app 1))
      sdl/K-PAGEUP    (move-lines app (- page))
      sdl/K-PAGEDOWN  (move-lines app page)
      sdl/K-V         (if cmd?
                        (let [s (-> (sdl/clipboard-text)
                                    (str/replace "\r\n" "\n")
                                    (str/replace "\r" "\n"))]
                          (if (seq s) (edit app ed/insert s) app))
                        app)
      app)))

(defn- on-click [app ev]
  (sdl/convert-event-to-render-coordinates (:renderer app) ev)
  (let [L (:layout app)
        m (px app margin)
        x (ffi/read ev :float sdl/O-button-x)
        y (ffi/read ev :float sdl/O-button-y)
        k (-> (long (Math/floor (/ (+ (- y m) (:scroll app)) (layout/line-height L))))
              (max 0)
              (min (dec (layout/line-count L))))]
    (move-to app (layout/position-at L k (- x m)))))

(defn- on-wheel [app ev]
  (let [dy (* (ffi/read ev :float sdl/O-wheel-y)
              (if (= sdl/MOUSEWHEEL-FLIPPED (ffi/read ev :uint sdl/O-wheel-direction)) -1 1))]
    (-> app
        (update :scroll #(- % (long (Math/round (* dy wheel-lines (layout/line-height (:layout app)))))))
        clamp-scroll
        (assoc :dirty? true))))

(defn- on-event [app ev]
  (let [type (ffi/read ev :uint sdl/O-event-type)]
    (condp = type
      sdl/EVENT-QUIT       (assoc app :quit? true)
      sdl/EVENT-TEXT-INPUT (edit app ed/insert (ffi/ptr->string (ffi/read ev :pointer sdl/O-text-text)))
      sdl/EVENT-KEY-DOWN   (on-key app (ffi/read ev :uint sdl/O-key-key)
                                   (ffi/read ev :uint16 sdl/O-key-mod))
      sdl/EVENT-MOUSE-BUTTON-DOWN (if (= sdl/BUTTON-LEFT (ffi/read ev :uint8 sdl/O-button-button))
                                    (on-click app ev)
                                    app)
      sdl/EVENT-MOUSE-WHEEL (on-wheel app ev)
      sdl/EVENT-WINDOW-FOCUS-GAINED (assoc app :focused? true :blink-from (sdl/get-ticks) :dirty? true)
      sdl/EVENT-WINDOW-FOCUS-LOST   (assoc app :focused? false :dirty? true)
      sdl/EVENT-WINDOW-EXPOSED      (assoc app :dirty? true)
      app)))

;; ---------------------------------------------------------------- drawing

(defn- caret-visible? [app now]
  (and (:focused? app) (even? (quot (- now (:blink-from app)) blink-ms))))

(defn- ms-until-blink
  "How long to sleep: until the caret next toggles, or indefinitely (-1)
  while unfocused, when nothing changes without an event."
  [app now]
  (if (:focused? app) (- blink-ms (mod (- now (:blink-from app)) blink-ms)) -1))

(defn- draw! [app {:keys [frect irect]} caret?]
  (let [{:keys [renderer layout scroll size textures]} app
        [w h] size
        {:keys [line-height baseline caret-top caret-height]} (:metrics layout)
        m  (px app margin)
        n  (layout/line-count layout)
        first-k (quot scroll line-height)
        last-k  (min n (inc (quot (+ scroll (view-height app)) line-height)))
        [br bg bb] background
        [fr fg fb] foreground]
    (sdl/set-render-draw-color renderer br bg bb 255)
    (sdl/render-clear renderer)
    (sdl/set-render-clip-rect renderer (sdl/set-rect! irect 0 m w (view-height app)))
    (doseq [k (range first-k last-k)
            :let [{:keys [line text]} (layout/visual-line layout k)]
            :when (not (str/blank? text))]
      (let [{:keys [texture width height pad] base :baseline}
            (textures/fetch! textures renderer text line foreground)
            y (+ m (- (* k line-height) scroll) (- baseline base))]
        (sdl/render-texture renderer texture ffi/null
                            (sdl/set-frect! frect (- m pad) y width height))))
    (let [[x k] (layout/caret layout (get-in app [:doc :caret]))
          cx (+ m (long (Math/floor x)))
          cy (+ m (- (* k line-height) scroll) caret-top)]
      (when caret?
        (sdl/set-render-draw-color renderer fr fg fb 255)
        (sdl/render-fill-rect renderer (sdl/set-frect! frect cx cy (max 1 (px app 1)) caret-height)))
      ;; Where the input method shows its candidate window, in window points.
      (let [d (:density app)]
        (sdl/set-text-input-area (:window app)
                                 (sdl/set-rect! irect (/ cx d) (/ cy d) 1 (/ caret-height d))
                                 0)))
    (sdl/set-render-clip-rect renderer ffi/null)
    (sdl/render-present renderer)
    (textures/end-frame! textures)))

;; ---------------------------------------------------------------- main

(defn- run-loop
  "Run until quit. `latest` always holds the current app, for cleanup."
  [latest scratch]
  (let [ev (:event scratch)
        drain (fn [app]
                (loop [app (on-event (sync-view app) ev)]
                  (if (sdl/poll-event ev) (recur (on-event (sync-view app) ev)) app)))]
    (loop [app @latest]
      (reset! latest app)
      (when-not (:quit? app)
        (let [app (if (sdl/wait-event-timeout ev (ms-until-blink app (sdl/get-ticks)))
                    (drain app)
                    app)
              app (sync-view app)
              app (clamp-scroll (if (:follow? app)
                                  (-> app follow-caret (assoc :follow? false))
                                  app))
              phase (caret-visible? app (sdl/get-ticks))]
          (if (or (:dirty? app) (not= phase (:drawn-phase app)))
            (do (draw! app scratch phase)
                (recur (assoc app :dirty? false :drawn-phase phase)))
            (recur app)))))))

(defn -main [& _args]
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
          app (atom {:window window :renderer renderer
                     :textures (textures/cache)
                     :doc ed/empty-doc :scroll 0
                     :focused? true :blink-from (sdl/get-ticks) :dirty? true})]
      (sdl/set-cursor cursor)
      (sdl/check! (sdl/start-text-input window) "SDL_StartTextInput")
      (try
        (with-open [a (ffi/confined-arena)]
          (swap! app sync-view)
          (run-loop app
                    {:event (ffi/alloc a sdl/EVENT-SIZE)
                     :frect (ffi/alloc a sdl/frect)
                     :irect (ffi/alloc a sdl/rect)}))
        (finally
          (release-view! @app)
          (sdl/destroy-cursor cursor)
          (sdl/destroy-renderer renderer)
          (sdl/destroy-window window))))
    (finally
      (sdl/quit))))
