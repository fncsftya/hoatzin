(ns hoatzin.app.draw.dropdown
  "Drawing the open dropdown list (see hoatzin.app.dropdown), over
  everything: the rows in view, scrolled to the pixel, the one it is on
  highlighted and the one the dropdown holds ticked, and a thumb showing
  how far down it is."
  (:require [hoatzin.app.boxes :refer [focused-field ui-value]]
            [hoatzin.app.draw.boxes :refer [draw-swatches! draw-text! intersect set-clip!
                                            swatch-width]]
            [hoatzin.app.dropdown :as dropdown]
            [hoatzin.app.face :refer [family-face]]
            [hoatzin.app.state :refer [px]]
            [hoatzin.lib.sdl :as sdl]))

(def ^:private tick "What marks the option the dropdown holds." "✓")
(def ^:private tick-width "Points for the column the tick is in." 20)
(def ^:private text-right "Points between an option's text and the list's right." 8)
(def ^:private thumb-width "Points." 4)
(def ^:private thumb-min "Points: the shortest the thumb gets." 12)

(defn draw-list!
  "The open dropdown list, if any. A dropdown with :fonts? shows each
  option, a font family's name, in that font, and with :swatches, the
  option's squares at the right of its row."
  [app]
  (when-let [{[x y w h] :rect [ix iy iw ih :as inner] :inner
              :keys [row-h rows options max-scroll]} (dropdown/place app)]
    (let [{:keys [renderer scratch]} app
          {:keys [node]} (focused-field app)
          value  (ui-value app node)
          {:keys [active scroll]} (:list app)
          scroll (long (Math/round (double scroll)))
          n      (count options)
          bw     (px app 1)
          tw     (px app tick-width)
          fore   (:foreground app)
          fill!  (fn [[r g b] x y w h]
                   (sdl/set-render-draw-color renderer r g b 255)
                   (sdl/render-fill-rect renderer (sdl/set-frect! (:frect scratch) x y w h)))]
      (fill! (:ui-field-background app) x y w h)
      (let [c (:ui-border app)]
        (fill! c x y w bw)
        (fill! c x (- (+ y h) bw) w bw)
        (fill! c x (+ y bw) bw (- h bw bw))
        (fill! c (- (+ x w) bw) (+ y bw) bw (- h bw bw)))
      (doseq [i (range (quot scroll row-h) (min n (inc (quot (+ scroll ih -1) row-h))))
              :let [ry  (- (+ iy (* i row-h)) scroll)
                    row (intersect inner [ix ry iw row-h])]
              :when row]
        (let [option (str (nth options i))
              colours (get (:swatches node) option)
              right (- (+ ix iw) (px app text-right))
              f      (if (:fonts? node) (family-face app option) (:ui app))]
          (when (= i active)
            (apply fill! (:ui-highlight app) row))
          (when (= option value)
            (draw-text! app (:ui app) [fore tick] tick [ix ry tw row-h] fore true row))
          (when (seq colours)
            (set-clip! app row)
            (draw-swatches! app colours right ry row-h))
          ;; each font's texture is its own, though the text and colour match
          (draw-text! app f [(when (:fonts? node) option) fore option] option
                      [(+ ix tw) ry (max 0 (- right ix tw (swatch-width app (count colours) row-h))) row-h]
                      fore false row)
          (set-clip! app nil)))
      (when (pos? max-scroll)
        (let [th (min ih (max (px app thumb-min) (quot (* ih rows) n)))
              ty (+ iy (long (Math/round (/ (* (- ih th) (double scroll)) max-scroll))))]
          (fill! (:scrollbar-thumb app) (- (+ ix iw) (px app thumb-width) (px app 2)) ty
                 (px app thumb-width) th))))))
