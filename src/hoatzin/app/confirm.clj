(ns hoatzin.app.confirm
  "Asking yes or no in the status bar: while a question is open, `y`
  answers yes, and any other letter, or escape, no."
  (:require [hoatzin.lib.sdl :as sdl]))

(defn ask
  "Ask `prompt` in the status bar; answered yes, the app becomes
  (`yes` app now)."
  [app prompt yes]
  (assoc app :confirm {:prompt prompt :yes yes} :dirty? true))

(defn prompt
  "The question open, or nil."
  [app]
  (:prompt (:confirm app)))

(defn on-event
  "An event while a question is open: typing answers it, escape answers
  no, and other keys wait for the answer. nil for the events it leaves to
  the rest of the app."
  [app now event]
  (let [{:keys [yes]} (:confirm app)
        answered (-> app (dissoc :confirm) (assoc :dirty? true))]
    (case (:type event)
      :text (if (#{"y" "Y"} (:text event)) (yes answered now) (assoc answered :message "Cancelled"))
      :key  (if (= sdl/K-ESCAPE (:key event)) (assoc answered :message "Cancelled") app)
      :composition app
      nil)))
