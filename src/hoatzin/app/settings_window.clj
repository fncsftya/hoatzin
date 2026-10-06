(ns hoatzin.app.settings-window
  "The settings window: a form over the text, opened by `:settings`. Its
  font family dropdowns and font size fields change their settings as
  they are chosen or edited, which applies and saves them straight away.
  The font dropdowns list the fonts installed, from the host's
  :font-families-fn, each name shown in its own font; the theme dropdown
  lists the themes, each with its colours."
  (:require [hoatzin.app.face :refer [ui-width]]
            [hoatzin.app.geometry :refer [status-height]]
            [hoatzin.app.settings :as settings]
            [hoatzin.app.theme :as theme]
            [hoatzin.lib.layout :as layout]))

(def fields
  "The settings window's fields and dropdowns, by :id: the :path of the
  setting each holds, and how to :parse it from what the box holds."
  {:settings/editor-family {:path [:editor-font :family] :parse identity}
   :settings/editor-size   {:path [:editor-font :size]   :parse parse-long}
   :settings/ui-family     {:path [:ui-font :family]     :parse identity}
   :settings/ui-size       {:path [:ui-font :size]       :parse parse-long}
   :settings/theme         {:path [:theme]               :parse identity}})

(defn open
  "Open the settings window, finding the fonts installed the first time."
  [app]
  (-> app
      (assoc :window :settings :dirty? true)
      (update :font-families #(or % (vec ((:font-families-fn app)))))))

(defn change-setting
  "Change the setting at `path` to `v`, and save the settings, if `v` may
  be it and is new. A change to the editor font applies once the setting
  has been left alone for :font-delay-ms, so that stepping or typing
  through sizes wraps the text only for the last."
  [app now path v]
  (let [s (settings/change (:settings app) path v)]
    (if (or (nil? s) (= s (:settings app)))
      app
      (let [app (cond-> (assoc app :settings s :dirty? true)
                  (= :editor-font (first path)) (assoc :fonts-at (+ now (:font-delay-ms app))))]
        (if-let [error ((:save-settings-fn app) s)]
          (assoc app :message (str "Can't save settings: " error))
          app)))))

(defn set-value
  "Interactive box `id` holds `value` now. A settings field or dropdown
  changes its setting, if `value` may be it."
  [app now id value]
  (let [app (-> app (assoc-in [:ui-values id] value) (assoc :dirty? true))]
    (if-let [{:keys [path parse]} (fields id)]
      (change-setting app now path (parse value))
      app)))

;; ---------------------------------------------------------------- the form

(def ^:private padding "Points inside the settings window's border." 16)
(def ^:private gap "Points between the settings window's rows." 10)
(def ^:private labels ["Editor font" "UI font" "Theme" "Line height"])
(def ^:private size-width "Points for the settings' font size fields." 40)

(defn window
  "The settings, as a form over the text, inset by the margin: a column
  of labels as wide as the widest, and their fields. The fonts can be
  changed (see `fields`); the line height is read-only for now."
  [app]
  (let [m      (:margin app)
        [lo hi] settings/font-sizes
        ;; the labels in a column as wide as the widest, in points
        label-w (/ (reduce max (map #(ui-width app %) labels)) (:density app))
        row    (fn [label & inputs]
                 {:kind :box :style {:direction :row :align :center :gap 8}
                  :children (into [{:kind :label :text label :style {:width label-w}}]
                                  inputs)})
        ;; fields fill their row, unless given a width
        shown  (fn [id value & [width]]
                 {:kind :field :id id :value (str value) :readonly? true
                  :style (if width {:width width} {:width 0 :grow 1})})
        ;; the boxes' ids are :settings/<id>-family and :settings/<id>-size
        font   (fn [label id setting]
                 (let [{:keys [family size]} (get-in app [:settings setting])]
                   (row label
                        {:kind :dropdown :id (keyword "settings" (str id "-family"))
                         :value family :options (:font-families app []) :fonts? true
                         :style {:width 0 :grow 1}}
                        {:kind :field :id (keyword "settings" (str id "-size"))
                         :value (str size) :input :integer :min lo :max hi
                         :style {:width size-width}})))]
    {:kind :box
     :style {:position :absolute :left m :top m :right m
             :bottom (+ (/ (status-height app) (:density app)) m)
             :padding padding :gap gap :border 1
             :background (:window-background app) :border-color (:ui-border app)}
     :children (let [[editor ui theme-label line-height] labels]
                 [{:kind :label :text "Settings" :style {:color (:foreground app)}}
                  ;; the key that closes the window, lightly dimmed
                  {:kind :label :text "esc"
                   :style {:position :absolute :top padding :right padding
                           :color (mapv #(quot (+ (* 2 %1) %2) 3)
                                        (:foreground app) (:window-background app))}}
                  (font editor "editor" :editor-font)
                  (font ui "ui" :ui-font)
                  (row theme-label {:kind :dropdown :id :settings/theme
                                    :value (:theme (:settings app)) :options theme/names
                                    :swatches (theme/swatches) :style {:width 0 :grow 1}})
                  (row line-height (shown :settings/line-height layout/line-spacing
                                          size-width))])}))
