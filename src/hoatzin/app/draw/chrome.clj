(ns hoatzin.app.draw.chrome
  "Drawing what is around the text: the scroll bar at its right and the
  status bar along the bottom."
  (:require [hoatzin.app.buffers :refer [buffer-name]]
            [hoatzin.app.command :as command]
            [hoatzin.app.confirm :as confirm]
            [hoatzin.app.face :refer [face-line ui-width]]
            [hoatzin.app.geometry :refer [text-height status-height]]
            [hoatzin.app.scroll :refer [scrollbar]]
            [hoatzin.app.state :refer [px]]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.textures :as textures]
            [jolt.ffi :as ffi]))

;; ---------------------------------------------------------------- scroll bar

(defn draw-scrollbar! [app]
  (when-let [{:keys [x w thumb-y thumb-h]} (scrollbar app)]
    (let [{:keys [renderer scratch]} app
          frect   (:frect scratch)
          active? (or (:hover? app) (:grab app))
          tw      (px app (if active? (:thumb-width-active app) (:thumb-width app)))
          tx      (- (+ x w) (px app 2) tw)
          [tr tg tb] (:scrollbar-track app)
          [r g b] (if active? (:scrollbar-thumb-active app) (:scrollbar-thumb app))]
      (when active?
        (sdl/set-render-draw-color renderer tr tg tb 255)
        (sdl/render-fill-rect renderer (sdl/set-frect! frect x 0 w (text-height app))))
      (sdl/set-render-draw-color renderer r g b 255)
      (sdl/render-fill-rect renderer (sdl/set-frect! frect tx thumb-y tw thumb-h)))))

;; ---------------------------------------------------------------- status bar

(def ^:private mode-labels {:normal "NORMAL" :insert "INSERT"})

(defn- status-text
  "What the status bar says on its left: the command line, a question, a
  message, or the mode."
  [{:keys [mode message] :as app}]
  (cond (= mode :command) (command/line-text app)
        (:confirm app)    (confirm/prompt app)
        message           message
        :else             (mode-labels mode)))

(defn- status-file
  "What the status bar says on its right: the buffer's name, [+] while
  the text differs from what is in its file, and its mode, if any."
  [{:keys [modified? major-mode] :as app}]
  (str (buffer-name app) (when modified? " [+]") (when major-mode (str " (" major-mode ")"))))

(defn- draw-status-text!
  "`text` in the status bar, starting at render pixel `x`."
  [app text x]
  (let [{:keys [renderer scratch ui ui-textures]} app
        color (:status-foreground app)
        {:keys [line]} (face-line ui text)
        {:keys [texture width height pad] base :baseline}
        (textures/fetch! ui-textures renderer [color text] line color)]
    (sdl/render-texture renderer texture ffi/null
                        (sdl/set-frect! (:frect scratch) (- x pad)
                                        (+ (text-height app) (px app (:status-padding app))
                                           (get-in ui [:metrics :baseline]) (- base))
                                        width height))))

(defn draw-status-bar!
  "The bar along the bottom: the mode, a message or the command line on
  the left, and the file on the right, unless the left runs into it."
  [app]
  (let [{:keys [renderer scratch]} app
        [w] (:size app)
        m (px app (:margin app))
        [r g b] (:status-background app)
        left  (status-text app)
        right (status-file app)
        right-x (- w m (ui-width app right))]
    (sdl/set-render-draw-color renderer r g b 255)
    (sdl/render-fill-rect renderer (sdl/set-frect! (:frect scratch) 0 (text-height app)
                                                   w (status-height app)))
    (draw-status-text! app left m)
    (when (< (+ m (ui-width app left) m) right-x)
      (draw-status-text! app right right-x))))
