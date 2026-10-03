(ns hoatzin.core
  (:require [jolt.ffi :as ffi]
            [hoatzin.coretext :as ct]
            [hoatzin.sdl :as sdl]))

(def title "Hello, World!")
(def font-size 48)        ; points; scaled by the window's pixel density

(defn- text-texture
  "Rasterize `text` with CoreText and upload it as an SDL texture.
  Returns {:texture t :width w :height h} in pixels."
  [renderer text px-size]
  (let [font (ct/font px-size)]
    (try
      (let [{:keys [pixels width height pitch]} (ct/rasterize font text)]
        (try
          (let [tex (sdl/check! (sdl/create-texture renderer sdl/PIXELFORMAT-RGBA32
                                                    sdl/TEXTUREACCESS-STATIC width height)
                                "SDL_CreateTexture")]
            (sdl/check! (sdl/update-texture tex ffi/null pixels pitch) "SDL_UpdateTexture")
            (sdl/check! (sdl/set-texture-blend-mode tex sdl/BLENDMODE-BLEND-PREMULTIPLIED)
                        "SDL_SetTextureBlendMode")
            {:texture tex :width width :height height})
          (finally (ffi/free pixels))))
      (finally (ct/release-font font)))))

(defn- draw! [renderer dst {:keys [texture width height]}]
  (sdl/set-render-draw-color renderer 30 30 46 255)
  (sdl/render-clear renderer)
  (let [[w h] (sdl/render-output-size renderer)]
    (ffi/write-field dst sdl/frect :x (float (Math/floor (/ (- w width) 2.0))))
    (ffi/write-field dst sdl/frect :y (float (Math/floor (/ (- h height) 2.0))))
    (ffi/write-field dst sdl/frect :w (float width))
    (ffi/write-field dst sdl/frect :h (float height))
    (sdl/set-texture-color-mod texture 245 240 230)
    (sdl/render-texture renderer texture ffi/null dst))
  (sdl/render-present renderer))

(defn- quit-requested?
  "Drain the event queue; true if the window was closed or Escape pressed."
  [ev]
  (loop [quit? false]
    (if-not (sdl/poll-event ev)
      quit?
      (let [type (ffi/read ev :uint sdl/O-event-type)]
        (recur (or quit?
                   (= type sdl/EVENT-QUIT)
                   (and (= type sdl/EVENT-KEY-DOWN)
                        (= (ffi/read ev :uint sdl/O-key-key) sdl/K-ESCAPE))))))))

(defn -main [& _args]
  (sdl/check! (sdl/init sdl/INIT-VIDEO) "SDL_Init")
  (try
    (let [[window renderer]
          (ffi/with-out [pwin :pointer]
            (ffi/with-out [pren :pointer]
              (sdl/check! (sdl/create-window-and-renderer
                           "Hoatzin" 640 480
                           (bit-or sdl/WINDOW-RESIZABLE sdl/WINDOW-HIGH-PIXEL-DENSITY)
                           pwin pren)
                          "SDL_CreateWindowAndRenderer")
              [(ffi/read pwin :pointer) (ffi/read pren :pointer)]))
          ;; The label is re-rasterized only when the pixel density changes
          ;; (e.g. the window moves between a Retina and a non-Retina display).
          label (atom nil)]
      (try
        (ffi/with-layout [dst sdl/frect]
          (ffi/with-alloc [ev sdl/EVENT-SIZE]
            (loop []
              (when-not (quit-requested? ev)
                (let [density (sdl/get-window-pixel-density window)]
                  (when-not (= density (:density @label))
                    (some-> @label :texture sdl/destroy-texture)
                    (reset! label (assoc (text-texture renderer title (* font-size density))
                                         :density density))))
                (draw! renderer dst @label)
                (sdl/delay 16)
                (recur)))))
        (finally
          (some-> @label :texture sdl/destroy-texture)
          (sdl/destroy-renderer renderer)
          (sdl/destroy-window window))))
    (finally
      (sdl/quit))))
