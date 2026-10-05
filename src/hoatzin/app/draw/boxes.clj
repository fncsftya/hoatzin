(ns hoatzin.app.draw.boxes
  "Drawing placed boxes (see hoatzin.lib.ui/place), in the UI font and
  the app's colours where their style sets none."
  (:require [hoatzin.app.face :refer [face-line face-width]]
            [hoatzin.app.input.fields :refer [ui-value]]
            [hoatzin.app.state :refer [px]]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.textures :as textures]
            [hoatzin.lib.ui :as ui]
            [jolt.ffi :as ffi]))

(defn- set-clip!
  "Clip drawing to [x y w h], or with nil not at all."
  [{:keys [renderer scratch]} clip]
  (sdl/set-render-clip-rect renderer (if-let [[x y w h] clip]
                                       (sdl/set-rect! (:irect scratch) x y w h)
                                       ffi/null)))

(defn- intersect
  "Rects [x y w h] `a` and `b` (nil: everywhere) overlap here, or nil."
  [a b]
  (if-not a
    b
    (let [[ax ay aw ah] a, [bx by bw bh] b
          x0 (max ax bx), y0 (max ay by)
          x1 (min (+ ax aw) (+ bx bw)), y1 (min (+ ay ah) (+ by bh))]
      (when (and (< x0 x1) (< y0 y1)) [x0 y0 (- x1 x0) (- y1 y0)]))))

(defn- draw-ui-text!
  "One line of `text` in `rect` [x y w h], in the UI font, clipped to the
  rect and to `clip`: centred vertically, and with `centre?` horizontally."
  [app text [x y w h :as rect] color centre? clip]
  (when-let [visible (and (seq text) (intersect clip rect))]
    (let [{:keys [renderer scratch ui ui-textures]} app
          {:keys [line-height baseline]} (:metrics ui)
          {:keys [line]} (face-line ui text)
          {:keys [texture width height pad] base :baseline}
          (textures/fetch! ui-textures renderer [color text] line color)
          x (if centre? (+ x (quot (- w (face-width ui text)) 2)) x)]
      (set-clip! app visible)
      (sdl/render-texture renderer texture ffi/null
                          (sdl/set-frect! (:frect scratch) (- x pad)
                                          (+ y (quot (- h line-height) 2) baseline (- base))
                                          width height))
      (set-clip! app clip))))

(defn draw-boxes!
  "Placed boxes moved by (`dx`, `dy`), within `clip` [x y w h] (nil: the
  whole window): each its background, its border, then what it holds."
  [app placed dx dy clip]
  (let [{:keys [renderer scratch]} app
        fill! (fn [[r g b] x y w h]
                (sdl/set-render-draw-color renderer r g b 255)
                (sdl/render-fill-rect renderer (sdl/set-frect! (:frect scratch) x y w h)))]
    (doseq [{:keys [node] [x y w h] :rect [cx cy cw ch] :content} placed
            :let [st (ui/style node)
                  x (+ x dx), y (+ y dy), cx (+ cx dx), cy (+ cy dy)
                  bw (px app (:border st 0))
                  focused? (and (:id node) (= (:id node) (:focus app)))
                  border (cond focused?          (:ui-focus app)
                               (:border-color st) (:border-color st)
                               :else             (:ui-border app))
                  color (cond (:color st)      (:color st)
                              (:readonly? node) (:status-foreground app)
                              :else            (:foreground app))
                  bg (or (:background st) (when (= :field (:kind node)) (:ui-field-background app)))]]
      (when bg (fill! bg x y w h))
      (when (pos? bw)
        (fill! border x y w bw)
        (fill! border x (- (+ y h) bw) w bw)
        (fill! border x (+ y bw) bw (- h bw bw))
        (fill! border (- (+ x w) bw) (+ y bw) bw (- h bw bw)))
      (case (:kind node)
        :label    (draw-ui-text! app (str (:text node)) [cx cy cw ch] color false clip)
        :button   (draw-ui-text! app (str (:text node)) [cx cy cw ch] color true clip)
        :field    (draw-ui-text! app (str (ui-value app node)) [cx cy cw ch] color false clip)
        :checkbox (when (ui-value app node)
                    (fill! (or (:color st) (:ui-accent app)) cx cy cw ch))
        nil))))
