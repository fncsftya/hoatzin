(ns hoatzin.sdl
  "Minimal SDL3 bindings: just enough to open a window and draw textures."
  (:require [jolt.ffi :as ffi]))

(def INIT-VIDEO 0x00000020)
(def WINDOW-RESIZABLE         0x0000000000000020)
(def WINDOW-HIGH-PIXEL-DENSITY 0x0000000000002000)

(def EVENT-QUIT 0x100)
(def EVENT-KEY-DOWN 0x300)
(def K-ESCAPE 0x1b)

;; SDL_Event is a 128-byte union; every member starts with a Uint32 type.
(def EVENT-SIZE 128)
(def O-event-type 0)
(def O-key-key 28)       ; SDL_KeyboardEvent.key (SDL_Keycode)

(def PIXELFORMAT-RGBA32 0x16762004)  ; ABGR8888: R,G,B,A bytes on little-endian
(def TEXTUREACCESS-STATIC 0)
(def BLENDMODE-BLEND-PREMULTIPLIED 0x10)

(def frect (ffi/layout [:struct [[:x :float] [:y :float] [:w :float] [:h :float]]]))

(ffi/defcfn init        "SDL_Init"        [:uint] :bool)
(ffi/defcfn quit        "SDL_Quit"        [] :void)
(ffi/defcfn get-error   "SDL_GetError"    [] :string)
(ffi/defcfn delay       "SDL_Delay"       [:uint] :void :blocking)
(ffi/defcfn poll-event  "SDL_PollEvent"   [:pointer] :bool)

(ffi/defcfn create-window-and-renderer "SDL_CreateWindowAndRenderer"
  [:string :int :int :uint64 :pointer :pointer] :bool)
(ffi/defcfn destroy-window   "SDL_DestroyWindow"   [:pointer] :void)
(ffi/defcfn destroy-renderer "SDL_DestroyRenderer" [:pointer] :void)
(ffi/defcfn get-window-pixel-density "SDL_GetWindowPixelDensity" [:pointer] :float)
(ffi/defcfn get-render-output-size "SDL_GetCurrentRenderOutputSize"
  [:pointer :pointer :pointer] :bool)

(ffi/defcfn set-render-draw-color "SDL_SetRenderDrawColor"
  [:pointer :uint8 :uint8 :uint8 :uint8] :bool)
(ffi/defcfn render-clear      "SDL_RenderClear"     [:pointer] :bool)
(ffi/defcfn render-present    "SDL_RenderPresent"   [:pointer] :bool)

(ffi/defcfn create-texture  "SDL_CreateTexture"  [:pointer :uint :int :int :int] :pointer)
(ffi/defcfn destroy-texture "SDL_DestroyTexture" [:pointer] :void)
(ffi/defcfn update-texture  "SDL_UpdateTexture"  [:pointer :pointer :pointer :int] :bool)
(ffi/defcfn set-texture-blend-mode "SDL_SetTextureBlendMode" [:pointer :uint] :bool)
(ffi/defcfn set-texture-color-mod  "SDL_SetTextureColorMod"
  [:pointer :uint8 :uint8 :uint8] :bool)
(ffi/defcfn render-texture "SDL_RenderTexture" [:pointer :pointer :pointer :pointer] :bool)

(defn check!
  "Throw with SDL_GetError when an SDL call failed (false or NULL)."
  [result what]
  (when (or (false? result) (and (number? result) (ffi/null? result)))
    (throw (ex-info (str what " failed: " (get-error)) {:sdl-error (get-error)})))
  result)

(defn render-output-size
  "The renderer's output size in pixels, as [w h]."
  [renderer]
  (ffi/with-out [pw :int]
    (ffi/with-out [ph :int]
      (check! (get-render-output-size renderer pw ph) "SDL_GetCurrentRenderOutputSize")
      [(ffi/read pw :int) (ffi/read ph :int)])))
