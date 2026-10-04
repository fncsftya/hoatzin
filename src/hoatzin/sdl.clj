(ns hoatzin.sdl
  "Minimal SDL3 bindings: a window, input events, and textured drawing."
  (:require [jolt.ffi :as ffi]))

(def INIT-VIDEO 0x00000020)
(def WINDOW-RESIZABLE          0x0000000000000020)
(def WINDOW-HIGH-PIXEL-DENSITY 0x0000000000002000)

;; Event types
(def EVENT-QUIT                  0x100)
(def EVENT-WINDOW-EXPOSED        0x204)
(def EVENT-WINDOW-MOUSE-LEAVE    0x20d)
(def EVENT-WINDOW-RESIZED        0x206)
(def EVENT-WINDOW-PIXEL-SIZE-CHANGED 0x207)
(def EVENT-WINDOW-FOCUS-GAINED   0x20e)
(def EVENT-WINDOW-FOCUS-LOST     0x20f)
(def EVENT-WINDOW-DISPLAY-SCALE-CHANGED 0x214)
(def EVENT-KEY-DOWN              0x300)
(def EVENT-TEXT-EDITING          0x302)
(def EVENT-TEXT-INPUT            0x303)
(def EVENT-MOUSE-MOTION          0x400)
(def EVENT-MOUSE-BUTTON-DOWN     0x401)
(def EVENT-MOUSE-BUTTON-UP       0x402)
(def EVENT-MOUSE-WHEEL           0x403)
(def EVENT-USER                  0x8000)

;; Keycodes and modifiers
(def K-RETURN    0x0d)
(def K-BACKSPACE 0x08)
(def K-DELETE    0x7f)
(def K-ESCAPE    0x1b)
(def K-TAB       0x09)
(def K-A         0x61)
(def K-C         0x63)
(def K-V         0x76)
(def K-X         0x78)
(def K-HOME      0x4000004a)
(def K-PAGEUP    0x4000004b)
(def K-END       0x4000004d)
(def K-PAGEDOWN  0x4000004e)
(def K-RIGHT     0x4000004f)
(def K-LEFT      0x40000050)
(def K-DOWN      0x40000051)
(def K-UP        0x40000052)
(def K-KP-ENTER  0x40000058)
(def KMOD-SHIFT  0x0003)
(def KMOD-GUI    0x0c00)

(def BUTTON-LEFT 1)
(def BUTTON-LMASK 1)
(def MOUSEWHEEL-FLIPPED 1)
(def SYSTEM-CURSOR-DEFAULT 0)
(def SYSTEM-CURSOR-TEXT 1)

;; SDL_Event is a 128-byte union; every member starts with a Uint32 type.
;; Offsets checked against the SDL 3.4 headers with offsetof.
(def EVENT-SIZE 128)
(def O-event-type 0)
(def O-key-key 28)        ; SDL_KeyboardEvent.key (SDL_Keycode)
(def O-key-mod 32)        ; SDL_KeyboardEvent.mod (SDL_Keymod, Uint16)
(def O-text-text 24)      ; SDL_TextInputEvent.text (const char *)
(def O-edit-text 24)      ; SDL_TextEditingEvent.text (const char *)
(def O-edit-start 32)     ; SDL_TextEditingEvent.start (Sint32, -1 if unset)
(def O-button-button 24)  ; SDL_MouseButtonEvent.button (Uint8)
(def O-button-clicks 26)  ; SDL_MouseButtonEvent.clicks (Uint8)
(def O-button-x 28)       ; SDL_MouseButtonEvent.x (float)
(def O-button-y 32)
(def O-motion-state 24)   ; SDL_MouseMotionEvent.state (SDL_MouseButtonFlags)
(def O-motion-x 28)       ; SDL_MouseMotionEvent.x (float)
(def O-motion-y 32)
(def O-wheel-y 28)        ; SDL_MouseWheelEvent.y (float)
(def O-wheel-direction 32)

(def PIXELFORMAT-RGBA32 0x16762004)  ; ABGR8888: R,G,B,A bytes on little-endian
(def TEXTUREACCESS-STATIC 0)
(def TEXTUREACCESS-TARGET 2)
(def BLENDMODE-BLEND-PREMULTIPLIED 0x10)

(def frect (ffi/layout [:struct [[:x :float] [:y :float] [:w :float] [:h :float]]]))
(def rect  (ffi/layout [:struct [[:x :int] [:y :int] [:w :int] [:h :int]]]))

(ffi/defcfn init        "SDL_Init"        [:uint] :bool)
(ffi/defcfn set-hint    "SDL_SetHint"     [:string :string] :bool)
(ffi/defcfn quit        "SDL_Quit"        [] :void)
(ffi/defcfn get-error   "SDL_GetError"    [] :string)
(ffi/defcfn get-ticks   "SDL_GetTicks"    [] :uint64)
(ffi/defcfn sdl-free    "SDL_free"        [:pointer] :void)
(ffi/defcfn poll-event  "SDL_PollEvent"   [:pointer] :bool)
(ffi/defcfn push-event  "SDL_PushEvent"   [:pointer] :bool)
(ffi/defcfn wait-event-timeout "SDL_WaitEventTimeout" [:pointer :int] :bool :blocking)
(ffi/defcfn convert-event-to-render-coordinates "SDL_ConvertEventToRenderCoordinates"
  [:pointer :pointer] :bool)

(ffi/defcfn create-window-and-renderer "SDL_CreateWindowAndRenderer"
  [:string :int :int :uint64 :pointer :pointer] :bool)
(ffi/defcfn destroy-window   "SDL_DestroyWindow"   [:pointer] :void)
(ffi/defcfn destroy-renderer "SDL_DestroyRenderer" [:pointer] :void)
(ffi/defcfn get-window-pixel-density "SDL_GetWindowPixelDensity" [:pointer] :float)
(ffi/defcfn get-render-output-size "SDL_GetCurrentRenderOutputSize"
  [:pointer :pointer :pointer] :bool)

(ffi/defcfn start-text-input    "SDL_StartTextInput"   [:pointer] :bool)
(ffi/defcfn set-text-input-area "SDL_SetTextInputArea" [:pointer :pointer :int] :bool)
(ffi/defcfn get-mod-state       "SDL_GetModState"      [] :uint16)
(ffi/defcfn get-clipboard-text* "SDL_GetClipboardText" [] :pointer)
(ffi/defcfn set-clipboard-text  "SDL_SetClipboardText" [:string] :bool)

;; The callback is void (*)(void *userdata, const char *const *filelist, int filter):
;; filelist is NULL on error, and empty (its first entry NULL) when cancelled.
(ffi/defcfn show-open-file-dialog "SDL_ShowOpenFileDialog"
  [:pointer :pointer :pointer :pointer :int :pointer :bool] :void :blocking)
(ffi/defcfn show-save-file-dialog "SDL_ShowSaveFileDialog"
  [:pointer :pointer :pointer :pointer :int :pointer] :void :blocking)

(ffi/defcfn create-system-cursor "SDL_CreateSystemCursor" [:int] :pointer)
(ffi/defcfn set-cursor           "SDL_SetCursor"          [:pointer] :bool)
(ffi/defcfn destroy-cursor       "SDL_DestroyCursor"      [:pointer] :void)

(ffi/defcfn set-render-draw-color "SDL_SetRenderDrawColor"
  [:pointer :uint8 :uint8 :uint8 :uint8] :bool)
(ffi/defcfn set-render-clip-rect "SDL_SetRenderClipRect" [:pointer :pointer] :bool)
(ffi/defcfn render-clear      "SDL_RenderClear"     [:pointer] :bool)
(ffi/defcfn render-fill-rect  "SDL_RenderFillRect"  [:pointer :pointer] :bool)
(ffi/defcfn render-present    "SDL_RenderPresent"   [:pointer] :bool)

(ffi/defcfn create-texture  "SDL_CreateTexture"  [:pointer :uint :int :int :int] :pointer)
(ffi/defcfn destroy-texture "SDL_DestroyTexture" [:pointer] :void)
(ffi/defcfn update-texture  "SDL_UpdateTexture"  [:pointer :pointer :pointer :int] :bool)
(ffi/defcfn set-texture-blend-mode "SDL_SetTextureBlendMode" [:pointer :uint] :bool)
(ffi/defcfn set-texture-color-mod "SDL_SetTextureColorMod"
  [:pointer :uint8 :uint8 :uint8] :bool)
(ffi/defcfn render-texture "SDL_RenderTexture" [:pointer :pointer :pointer :pointer] :bool)

;; Offscreen rendering and image files (used by the tests).
(ffi/defcfn create-software-renderer "SDL_CreateSoftwareRenderer" [:pointer] :pointer)
(ffi/defcfn set-render-target  "SDL_SetRenderTarget"  [:pointer :pointer] :bool)
(ffi/defcfn render-read-pixels "SDL_RenderReadPixels" [:pointer :pointer] :pointer)
(ffi/defcfn create-surface     "SDL_CreateSurface"    [:int :int :uint] :pointer)
(ffi/defcfn create-surface-from "SDL_CreateSurfaceFrom"
  [:int :int :uint :pointer :int] :pointer)
(ffi/defcfn convert-surface    "SDL_ConvertSurface"   [:pointer :uint] :pointer)
(ffi/defcfn destroy-surface    "SDL_DestroySurface"   [:pointer] :void)
(ffi/defcfn save-png           "SDL_SavePNG"          [:pointer :string] :bool)
(ffi/defcfn load-png           "SDL_LoadPNG"          [:string] :pointer)

;; SDL_Surface fields, from offsetof against SDL 3.4.
(def O-surface-w 8)
(def O-surface-h 12)
(def O-surface-pitch 16)
(def O-surface-pixels 24)

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

(defn clipboard-text
  "The clipboard's text, or \"\" when it holds none."
  []
  (let [p (get-clipboard-text*)]
    (if (ffi/null? p)
      ""
      (try (ffi/ptr->string p) (finally (sdl-free p))))))

(defn set-frect! [p x y w h]
  (ffi/write-field p frect :x (float x))
  (ffi/write-field p frect :y (float y))
  (ffi/write-field p frect :w (float w))
  (ffi/write-field p frect :h (float h))
  p)

(defn set-rect! [p x y w h]
  (ffi/write-field p rect :x (int x))
  (ffi/write-field p rect :y (int y))
  (ffi/write-field p rect :w (int w))
  (ffi/write-field p rect :h (int h))
  p)
