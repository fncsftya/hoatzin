(ns hoatzin.app.draw.boxes
  "Drawing placed boxes (see hoatzin.lib.ui/place), in the UI font and
  the app's colours where their style sets none."
  (:require [hoatzin.app.boxes :refer [ui-value]]
            [hoatzin.app.face :refer [face-line face-width family-face]]
            [hoatzin.app.state :refer [px]]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.textures :as textures]
            [hoatzin.lib.ui :as ui]
            [jolt.ffi :as ffi]))

(defn set-clip!
  "Clip drawing to [x y w h], or with nil not at all."
  [{:keys [renderer scratch]} clip]
  (sdl/set-render-clip-rect renderer (if-let [[x y w h] clip]
                                       (sdl/set-rect! (:irect scratch) x y w h)
                                       ffi/null)))

(defn intersect
  "Rects [x y w h] `a` and `b` (nil: everywhere) overlap here, or nil."
  [a b]
  (if-not a
    b
    (let [[ax ay aw ah] a, [bx by bw bh] b
          x0 (max ax bx), y0 (max ay by)
          x1 (min (+ ax aw) (+ bx bw)), y1 (min (+ ay ah) (+ by bh))]
      (when (and (< x0 x1) (< y0 y1)) [x0 y0 (- x1 x0) (- y1 y0)]))))

(defn draw-text!
  "One line of `text` in `rect` [x y w h], in face `f`, clipped to the
  rect and to `clip`: centred vertically, and with `centre?` horizontally.
  Its texture is cached as `key`, which must tell it from any other text
  in any other face or colour."
  [app f key text [x y w h :as rect] color centre? clip]
  (when-let [visible (and (seq text) (intersect clip rect))]
    (let [{:keys [renderer scratch ui-textures]} app
          {:keys [line-height baseline]} (:metrics f)
          {:keys [line]} (face-line f text)
          {:keys [texture width height pad] base :baseline}
          (textures/fetch! ui-textures renderer key line color)
          x (if centre? (+ x (quot (- w (face-width f text)) 2)) x)]
      (set-clip! app visible)
      (sdl/render-texture renderer texture ffi/null
                          (sdl/set-frect! (:frect scratch) (- x pad)
                                          (+ y (quot (- h line-height) 2) baseline (- base))
                                          width height))
      (set-clip! app clip))))

(defn- draw-ui-text!
  "`draw-text!` in the UI font."
  [app text rect color centre? clip]
  (draw-text! app (:ui app) [color text] text rect color centre? clip))

(def ^:private swatch-gap "Points between squares, and before the first." 3)

(defn swatch-width
  "How wide `n` swatches are, with a gap before each, in render pixels, for
  rows `h` high."
  [app n h]
  (* n (+ (px app swatch-gap) (quot (* 3 h) 4))))

(defn draw-swatches!
  "Squares of `colours`, outlined, in a row ending at render pixel `right`
  and centred in `h` from `y`."
  [app colours right y h]
  (let [{:keys [renderer scratch]} app
        side (quot (* 3 h) 4)
        gap  (px app swatch-gap)
        bw   (px app 1)
        top  (+ y (quot (- h side) 2))
        n    (count colours)
        fill! (fn [[r g b] x y w h]
                (sdl/set-render-draw-color renderer r g b 255)
                (sdl/render-fill-rect renderer (sdl/set-frect! (:frect scratch) x y w h)))]
    (doseq [[i colour] (map-indexed vector colours)
            :let [x (+ (- right (* (- n i) (+ side gap))) gap)]]
      (fill! (:ui-border app) x top side side)
      (fill! colour (+ x bw) (+ top bw) (- side bw bw) (- side bw bw)))))

(def ^:private arrow "What a dropdown shows at its right." "▾")
(def ^:private arrow-gap "Points between a dropdown's value and its arrow." 4)

(defn- draw-dropdown!
  "A dropdown's value, and its arrow at the right in the UI font. The
  value is in the UI font too, unless the dropdown has :fonts?: then it
  is in the font family it names. With :swatches, the value's squares are
  between the two."
  [app node [cx cy cw ch] color clip]
  (let [aw    (min cw (face-width (:ui app) arrow))
        value (str (ui-value app node))
        colours (get (:swatches node) value)
        right (- (+ cx cw) aw (px app arrow-gap))
        sw    (swatch-width app (count colours) ch)
        rect  [cx cy (max 0 (- right cx sw)) ch]]
    (when (seq colours) (draw-swatches! app colours right cy ch))
    (if (:fonts? node)
      (draw-text! app (family-face app value) [value color value] value rect color false clip)
      (draw-ui-text! app value rect color false clip))
    (draw-ui-text! app arrow [(- (+ cx cw) aw) cy aw ch] (:ui-dim app) true clip)))

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
                              (:readonly? node) (:ui-dim app)
                              :else            (:foreground app))
                  hovered? (and (:id node) (= (:id node) (:ui-hover-id app)))
                  bg (or (:background st)
                         (case (:kind node)
                           :field    (:ui-field-background app)
                           :dropdown (if hovered? (:ui-hover app) (:ui-field-background app))
                           nil))]]
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
        :dropdown (draw-dropdown! app node [cx cy cw ch] color clip)
        :checkbox (when (ui-value app node)
                    (fill! (or (:color st) (:ui-accent app)) cx cy cw ch))
        nil))))
