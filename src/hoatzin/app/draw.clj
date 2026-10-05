(ns hoatzin.app.draw
  "Drawing the app, back to front: the text and the blocks in it, clipped
  to the text area; the scroll bar and the status bar; then the floats,
  and an open dropdown list over them."
  (:require [hoatzin.app.caret :refer [caret-visible?]]
            [hoatzin.app.draw.boxes :refer [draw-boxes!]]
            [hoatzin.app.draw.caret :refer [draw-bar-caret! draw-block-caret!]]
            [hoatzin.app.draw.chrome :refer [draw-scrollbar! draw-status-bar!]]
            [hoatzin.app.draw.dropdown :refer [draw-list!]]
            [hoatzin.app.draw.text :refer [draw-selection! draw-lines! draw-composition!]]
            [hoatzin.app.geometry :refer [view-height visible-lines]]
            [hoatzin.app.state :refer [px insert? command?]]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.textures :as textures]
            [jolt.ffi :as ffi]))

(defn- draw-blocks!
  "The blocks at least partly in view, clipped to the text area `clip`."
  [app clip]
  (let [{:keys [scroll]} app
        m  (px app (:margin app))
        vh (view-height app)]
    (doseq [{:keys [top height placed]} (:block-places app)
            :when (and (< top (+ scroll vh)) (> (+ top height) scroll))]
      (draw-boxes! app placed m (- m scroll) clip))))

(defn draw!
  "Render the app at time `now` (ms), present it, and return the app."
  [app now]
  (let [{:keys [renderer size scratch]} app
        [w _] size
        m  (px app (:margin app))
        vh (view-height app)
        [first-k last-k] (visible-lines app)
        caret?  (caret-visible? app now)
        [br bg bb] (:background app)]
    (sdl/set-render-draw-color renderer br bg bb 255)
    (sdl/render-clear renderer)
    ;; the text area
    (sdl/set-render-clip-rect renderer (sdl/set-rect! (:irect scratch) 0 m w vh))
    (draw-selection! app first-k last-k)
    (draw-lines! app first-k last-k)
    (draw-composition! app first-k last-k)
    (draw-blocks! app [0 m w vh])
    (when (and caret? (not (command? app)) (not (:focus app)))
      (if (insert? app) (draw-bar-caret! app) (draw-block-caret! app)))
    (sdl/set-render-clip-rect renderer ffi/null)
    ;; around it, and over it
    (draw-scrollbar! app)
    (draw-status-bar! app)
    (when (and caret? (command? app))
      (draw-bar-caret! app))
    (draw-boxes! app (:float-places app) 0 0 nil)
    (draw-list! app)
    (when (and caret? (:focus app))
      (draw-bar-caret! app))
    (sdl/render-present renderer)
    (textures/end-frame! (:textures app))
    (textures/end-frame! (:ui-textures app))
    (assoc app :dirty? false :drawn-phase caret?)))
