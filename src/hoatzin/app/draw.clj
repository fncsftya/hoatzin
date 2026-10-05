(ns hoatzin.app.draw
  "Drawing the app, back to front: the text, what its minor modes show
  over it (see hoatzin.app.modes) and the blocks in it, clipped
  to the text area; the scroll bar and the status bar; then the floats,
  and an open dropdown list over them."
  (:require [hoatzin.app.caret :refer [caret-visible?]]
            [hoatzin.app.draw.boxes :refer [draw-boxes! draw-text! intersect set-clip!]]
            [hoatzin.app.draw.caret :refer [draw-bar-caret! draw-block-caret!]]
            [hoatzin.app.draw.chrome :refer [draw-scrollbar! draw-status-bar!]]
            [hoatzin.app.draw.dropdown :refer [draw-list!]]
            [hoatzin.app.draw.text :refer [draw-selection! draw-lines! draw-composition!]]
            [hoatzin.app.geometry :refer [line-top origin view-height visible-lines]]
            [hoatzin.app.insets :as insets]
            [hoatzin.app.modes :as modes]
            [hoatzin.app.state :refer [px insert? command?]]
            [hoatzin.lib.layout :as layout]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.textures :as textures]
            [jolt.ffi :as ffi]))

(declare draw-blocks!)

(defn- draw-markers!
  "The bullets, or boxes, before the items in view of list `i`, seen as
  view `v`, in the gutter at the left of its text."
  [v i k0 k1]
  (let [{:keys [renderer scratch scroll]} v
        [ox oy] (origin v)
        lh   (layout/line-height (:layout v))
        gw   (px v insets/gutter)
        side (px v 10)
        bw   (max 1 (px v 1))
        fill! (fn [[r g b] x y w h]
                (sdl/set-render-draw-color renderer r g b 255)
                (sdl/render-fill-rect renderer (sdl/set-frect! (:frect scratch) x y w h)))]
    (doseq [[k checked?] (insets/markers v i k0 k1)
            :let [y (+ oy (- (line-top v k) scroll))]]
      (if (nil? checked?)
        (draw-text! v (:ui v) [:bullet (:foreground v)] "•" [(- ox gw) y gw lh] (:foreground v) true (:clip v))
        (let [x (+ (- ox gw) (quot (- gw side) 2))
              y (+ y (quot (- lh side) 2))]
          (fill! (:ui-border v) x y side side)
          (fill! (if checked? (:ui-accent v) (:background v))
                 (+ x bw) (+ y bw) (- side bw bw) (- side bw bw)))))))

(defn- draw-inset!
  "The text of `level`'s inset `b`, as placed, if it is unfolded and
  shows: its selection, its lines, any composition and its own insets,
  within where it shows; and its scroll thumb if it scrolls, within
  `clip`."
  [level b clip]
  (when-let [v (insets/view level (:inset b))]
    (when-let [c (:clip v)]
      (let [[k0 k1] (visible-lines v)
            i (get-in level [:insets (:inset b)])]
        (set-clip! level c)
        (draw-selection! v k0 k1)
        (draw-lines! v k0 k1)
        (draw-composition! v k0 k1)
        (draw-blocks! v c)
        (when-not (insets/section? i)
          ;; the gutter is left of the text, outside where it shows
          (let [[_ cy _ ch] c
                [ox] (origin v)
                gw (px v insets/gutter)]
            (when-let [gc (intersect clip [(- ox gw) cy gw ch])]
              (set-clip! level gc)
              (draw-markers! (assoc v :clip gc) i k0 k1))))
        (set-clip! level clip)))
    (when-let [[x y w h] (insets/thumb level (get-in level [:insets (:inset b)]) b)]
      (let [{:keys [renderer scratch scroll]} level
            [ox oy] (origin level)
            [r g b] (:scrollbar-thumb level)]
        (sdl/set-render-draw-color renderer r g b 255)
        (sdl/render-fill-rect renderer (sdl/set-frect! (:frect scratch) (+ ox x) (+ oy (- y scroll)) w h))))))

(defn- draw-blocks!
  "`level`'s blocks at least partly in view, clipped to `clip`, where its
  text shows, and the insets' text in them."
  [level clip]
  (let [{:keys [scroll]} level
        [ox oy] (origin level)
        vh (view-height level)]
    (doseq [{:keys [top height placed] :as b} (:block-places level)
            :when (and (< top (+ scroll vh)) (> (+ top height) scroll))]
      (draw-boxes! level placed ox (- oy scroll) clip)
      (when (:inset b) (draw-inset! level b clip)))))

(defn- draw-text-caret!
  "The caret in the text, or in the inset it is in, within `clip`: a bar
  in insert mode, a block in normal mode; a bar in the title of a section
  being renamed; none over a folded section, whose header shows it."
  [app clip]
  (cond
    (:renaming app) (do (set-clip! app clip) (draw-bar-caret! app))
    (insets/over app) nil
    :else (let [t (insets/innermost-view app)]
            (when-let [c (if (= t app) clip (:clip t))]
              (set-clip! app c)
              (if (insert? t) (draw-bar-caret! t) (draw-block-caret! t))))))

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
    (modes/draw-under! app first-k last-k)
    (draw-lines! app first-k last-k)
    (draw-composition! app first-k last-k)
    (modes/draw! app first-k last-k)
    (draw-blocks! app [0 m w vh])
    (when (and caret? (not (command? app)) (not (:focus app)))
      (draw-text-caret! app [0 m w vh]))
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
