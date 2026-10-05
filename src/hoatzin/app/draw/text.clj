(ns hoatzin.app.draw.text
  "Drawing the text's visual lines [`first-k`, `last-k`): the selection
  behind them, the lines, and the underline of any composition."
  (:require [clojure.string :as str]
            [hoatzin.app.geometry :refer [line-top]]
            [hoatzin.app.state :refer [px]]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.layout :as layout]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.textures :as textures]
            [jolt.ffi :as ffi]))

(defn draw-selection! [app first-k last-k]
  (when-let [[lo hi] (ed/selection (:doc app))]
    (let [{:keys [renderer layout scroll scratch]} app
          {:keys [line-height caret-height]} (:metrics layout)
          m (px app (:margin app))
          [r g b] (if (:focused? app) (:selection app) (:selection-unfocused app))
          nl (max 1 (quot caret-height 4))]
      (sdl/set-render-draw-color renderer r g b 255)
      (doseq [[k x0 x1] (layout/selection-segments layout lo hi nl first-k last-k)
              :let [x0 (long (Math/floor x0))]]
        (sdl/render-fill-rect renderer
                              (sdl/set-frect! (:frect scratch)
                                              (+ m x0) (+ m (- (line-top app k) scroll))
                                              (- (long (Math/ceil x1)) x0) line-height))))))

(defn draw-lines! [app first-k last-k]
  (let [{:keys [renderer layout scroll textures scratch]} app
        {:keys [baseline]} (:metrics layout)
        m (px app (:margin app))]
    (doseq [k (range first-k last-k)
            :let [{:keys [line text]} (layout/visual-line layout k)]
            :when (not (str/blank? text))]
      (let [{:keys [texture width height pad] base :baseline}
            (textures/fetch! textures renderer text line (:foreground app))
            y (+ m (- (line-top app k) scroll) (- baseline base))]
        (sdl/render-texture renderer texture ffi/null
                            (sdl/set-frect! (:frect scratch) (- m pad) y width height))))))

(defn draw-composition!
  "The composition's underline, just below the baseline."
  [app first-k last-k]
  (when-let [{comp :text} (:composition app)]
    (let [{:keys [renderer layout scroll scratch]} app
          {:keys [baseline]} (:metrics layout)
          m (px app (:margin app))
          [fr fg fb] (:foreground app)
          a (get-in app [:doc :caret])
          thickness (max 1 (px app 1))
          below     (max 1 (px app 2))]
      (sdl/set-render-draw-color renderer fr fg fb 255)
      (doseq [[k x0 x1] (layout/range-segments layout a (+ a (count comp)) first-k last-k)]
        (sdl/render-fill-rect renderer
                              (sdl/set-frect! (:frect scratch)
                                              (+ m (long (Math/floor x0)))
                                              (+ m (- (line-top app k) scroll) baseline below)
                                              (- (long (Math/ceil x1)) (long (Math/floor x0)))
                                              thickness))))))
