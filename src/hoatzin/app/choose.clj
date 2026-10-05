(ns hoatzin.app.choose
  "Choosing from a numbered list of options: while one is open, a box
  above the status bar lists them, `1. markdown   2. html`, typing a
  number chooses it, and escape, or any other text, chooses none."
  (:require [clojure.string :as str]
            [hoatzin.app.hints :as hints]
            [hoatzin.lib.sdl :as sdl]))

(defn ask
  "Ask for one of `options`, strings; the one chosen, and the app, go to
  (`chosen` app now option)."
  [app options chosen]
  (assoc app :choose {:options (vec options) :chosen chosen} :dirty? true))

(defn prompt
  "What the status bar says while a choice is open, or nil."
  [app]
  (when (:choose app) "Choose a number, esc cancels"))

(defn on-event
  "An event while a choice is open: a number chooses, other text and
  escape cancel, and other keys wait. nil for the events it leaves to the
  rest of the app."
  [app now event]
  (let [{:keys [options chosen]} (:choose app)
        answered (-> app (dissoc :choose) (assoc :dirty? true))]
    (case (:type event)
      :text (let [n (when (re-matches #"[1-9]" (str/trim (:text event))) (parse-long (str/trim (:text event))))]
              (if (and n (<= n (count options)))
                (chosen answered now (options (dec n)))
                (assoc answered :message "Cancelled")))
      :key  (if (= sdl/K-ESCAPE (:key event)) (assoc answered :message "Cancelled") app)
      :composition app
      nil)))

(defn hints
  "The float listing the options open, or nil."
  [app]
  (when-let [{:keys [options]} (:choose app)]
    (hints/box app
               (map-indexed (fn [i name]
                              (let [n (str (inc i) ".")]
                                {:text (str n " " name)
                                 :node {:kind :box :style {:direction :row :gap 6}
                                        :children [{:kind :label :text n :style {:color (:ui-accent app)}}
                                                   {:kind :label :text name :style {:color (:foreground app)}}]}}))
                            options))))
