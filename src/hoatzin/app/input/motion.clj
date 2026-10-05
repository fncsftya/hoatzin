(ns hoatzin.app.input.motion
  "Moving the caret and the selection, as the keyboard and the mouse both
  do."
  (:require [hoatzin.app.geometry :refer [caret-or]]
            [hoatzin.app.state :refer [touched]]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.layout :as layout]))

(defn move-to
  "Move the caret to `pos`, or with `extend?` extend the selection to it."
  ([app now extend? pos] (move-to app now extend? pos nil false))
  ([app now extend? pos goal-x upstream?]
   (-> app
       (assoc :doc ((if extend? ed/select ed/move) (:doc app) pos)
              :goal-x goal-x :upstream? upstream?)
       (touched now))))

(defn move-on-line
  "Move (or extend) to `pos` on visual line `k`, keeping the caret on that
  line if `pos` is where it wraps."
  ([app now extend? k pos] (move-on-line app now extend? k pos nil))
  ([app now extend? k pos goal-x]
   (move-to app now extend? pos goal-x (layout/wrap-end? (:layout app) k pos))))

(defn move-lines
  "Move `n` visual lines from `from`, aiming for the remembered column."
  [app now extend? from n]
  (let [L (:layout app)
        [x k] (caret-or app from)
        goal (or (when (= from (get-in app [:doc :caret])) (:goal-x app)) x)
        k2 (+ k n)]
    (cond
      (neg? k2) (move-to app now extend? 0)
      (>= k2 (layout/line-count L)) (move-to app now extend? (count (get-in app [:doc :text])))
      :else (move-on-line app now extend? k2 (layout/position-at L k2 goal) goal))))

(defn select-range
  "Select from `anchor` to `pos`."
  [app now anchor pos]
  (-> app
      (assoc :doc (ed/select (ed/move (:doc app) anchor) pos) :goal-x nil :upstream? false)
      (touched now)))
