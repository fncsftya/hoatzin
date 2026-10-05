(ns hoatzin.app.draw.caret
  "Drawing the caret where hoatzin.app.caret puts it."
  (:require [clojure.string :as str]
            [hoatzin.app.caret :refer [caret-rect]]
            [hoatzin.app.geometry :as geo]
            [hoatzin.app.state :refer [px]]
            [hoatzin.lib.layout :as layout]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.textures :as textures]
            [jolt.ffi :as ffi]))

(defn draw-block-caret!
  "The block caret: solid foreground, with the text under it redrawn in the
  background colour so it reads inverted. That is a texture of its own,
  keyed apart from the line's, in the background colour: modulating the
  line's, in the foreground colour, can't lighten black text."
  [app]
  (let [{:keys [renderer textures scratch layout scroll]} app
        {:keys [frect irect]} scratch
        {:keys [baseline]} (:metrics layout)
        m  (px app (:margin app))
        [x y w h] (caret-rect app)
        [fr fg fb] (:foreground app)
        back (:background app)
        [_ k] (geo/caret-place app)
        {:keys [line text]} (layout/visual-line layout k)
        ;; the part of the block in view; the text area's clip hides the rest
        top (max y m)
        bottom (min (+ y h) (+ m (geo/view-height app)))]
    (sdl/set-render-draw-color renderer fr fg fb 255)
    (sdl/render-fill-rect renderer (sdl/set-frect! frect x y w h))
    (when (and (not (str/blank? text)) (< top bottom))
      (let [{:keys [texture width height pad] base :baseline}
            (textures/fetch! textures renderer [:caret text] line back)]
        (sdl/set-render-clip-rect renderer (sdl/set-rect! irect x top w (- bottom top)))
        (sdl/render-texture renderer texture ffi/null
                            (sdl/set-frect! frect (- m pad)
                                            (+ m (- (geo/line-top app k) scroll) (- baseline base))
                                            width height))))))

(defn draw-bar-caret! [{:keys [renderer scratch] :as app}]
  (let [[x y w h] (caret-rect app)
        [r g b] (:foreground app)]
    (sdl/set-render-draw-color renderer r g b 255)
    (sdl/render-fill-rect renderer (sdl/set-frect! (:frect scratch) x y w h))))
