(ns hoatzin.app.rename
  "Renaming a section in its header (see hoatzin.app.insets/start-rename):
  typing adds to the name, backspace takes from its end, return, up or
  down keep it, and escape gives it up. A click elsewhere keeps it too."
  (:require [hoatzin.app.insets :as insets]
            [hoatzin.lib.sdl :as sdl]))

(defn on-event
  "An event while a section is being renamed. nil for the events it
  leaves to the rest of the app, once it has kept the name: see `keep`."
  [app event]
  (case (:type event)
    :text (insets/rename app #(str % (:text event)))
    :key  (condp contains? (:key event)
            #{sdl/K-RETURN sdl/K-KP-ENTER sdl/K-UP sdl/K-DOWN} (insets/end-rename app true)
            #{sdl/K-ESCAPE}    (insets/end-rename app false)
            #{sdl/K-BACKSPACE} (insets/rename app #(subs % 0 (max 0 (dec (count %)))))
            app)
    :composition app
    nil))

(defn keep-for
  "The app as `event` finds it: a click keeps the name being typed, and
  goes on to where it is."
  [app event]
  (if (and (:renaming app) (= :click (:type event))) (insets/end-rename app true) app))
