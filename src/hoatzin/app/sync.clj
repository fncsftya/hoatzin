(ns hoatzin.app.sync
  "Bringing what the app shows up to date with what it holds: the fonts
  and colours with the settings and the density, the layout with the text and the
  window's size, and the boxes' places with both."
  (:require [hoatzin.app.boxes :as boxes]
            [hoatzin.app.display :as display]
            [hoatzin.app.face :as face]
            [hoatzin.app.files :as files]
            [hoatzin.app.insets :as insets]
            [hoatzin.app.scroll :as scroll]
            [hoatzin.app.state :refer [px]]
            [hoatzin.app.theme :as theme]
            [hoatzin.lib.coretext :as ct]
            [hoatzin.lib.layout :as layout]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.textures :as textures]))

(defn- release-editor-view! [{:keys [ctx textures font] :as app}]
  (some-> ctx layout/release-context)
  (insets/release-contexts! app)
  (some-> textures textures/clear!)
  (some-> font ct/release-font))

(defn- release-ui-view! [{:keys [ui ui-textures option-faces]}]
  (some-> ui-textures textures/clear!)
  (some-> option-faces face/release-family-faces!)
  (some-> ui face/release-face!))

(defn release-view!
  "Release the fonts, layout context and textures `sync-view` made."
  [app]
  (release-editor-view! app)
  (release-ui-view! app))

(defn- sync-fonts
  "Make each font again when it, or the density, has changed: the editor's
  means wrapping the whole text again. While the editor font waits (see
  `:fonts-at`), it stays as it is."
  [app]
  (let [density (double ((:density-fn app)))
        {:keys [editor-font ui-font]} (:settings app)
        editor-font (if (:fonts-at app) (get-in app [:fonts :editor-font]) editor-font)
        new-density? (not= density (:density app))
        app (assoc app :density density)
        app (if (and (not new-density?) (= editor-font (get-in app [:fonts :editor-font])))
              app
              (do (release-editor-view! app)
                  (-> app
                      (assoc :font (ct/font (:family editor-font) (* (:size editor-font) density))
                             :ctx nil :inset-ctxs {} :dirty? true)
                      (assoc-in [:fonts :editor-font] editor-font))))]
    (if (and (not new-density?) (= ui-font (get-in app [:fonts :ui-font])))
      app
      (do (release-ui-view! app)
          (-> app
              (assoc :ui (face/face app (:family ui-font) (:size ui-font) face/ui-lines-kept)
                     :dirty? true)
              (assoc-in [:fonts :ui-font] ui-font))))))

(defn- sync-theme
  "Take the colours of the theme the settings name, when it has changed.
  The line textures of the text are cached by text alone, so they go."
  [app]
  (let [name (:theme (:settings app))]
    (if (= name (:applied-theme app))
      app
      (do (some-> (:textures app) textures/clear!)
          (-> (merge app (theme/colours name))
              (assoc :applied-theme name :dirty? true))))))

(defn- sync-size
  "Take the renderer's output size, and wrap the text, and the insets'
  text, to fit it. Given the time, `now`, a text longer than
  :live-wrap-chars keeps the width it is wrapped to until a new one has
  held for :wrap-delay-ms, as it does while the window is dragged: until
  then the text shows as it was wrapped, within the window, and
  `ms-until-wake` wakes the app for it."
  [app now]
  (let [[w _ :as size] (sdl/render-output-size (:renderer app))
        wrap  (max 1 (- w (* 2 (px app (:margin app)))))
        wrapped (get-in app [:ctx :width])
        wait? (and now wrapped (> (count (get-in app [:doc :text])) (:live-wrap-chars app)))
        app (cond
              (= wrap wrapped)
              (dissoc app :wrap-width :wrap-at)

              (and wait? (not= wrap (:wrap-width app)))
              (assoc app :wrap-width wrap :wrap-at (+ now (:wrap-delay-ms app)))

              (and wait? (< now (:wrap-at app)))
              app

              :else
              (do (some-> (:ctx app) layout/release-context)
                  (insets/release-contexts! app)
                  ;; Re-wrapping moves every line; keep the caret's in view.
                  (-> app
                      (dissoc :wrap-width :wrap-at)
                      (assoc :ctx (layout/context (:font app) wrap) :layout nil :follow? true
                             :inset-ctxs {}))))]
    (if (= size (:size app)) app (assoc app :size size :dirty? true))))

(defn- sync-layout
  "Lay the text out again, if it has changed since it last was."
  [app]
  (let [app   (insets/sync-levels app)
        shown (display/display-key app)]
    (if (and (:layout app) (identical? (:levels app) (:laid-levels app))
             (display/same-display? shown (:laid-out app)))
      app
      (assoc app :layout (layout/layout (:ctx app) (display/display-text app) (:levels app))
             :laid-out shown :laid-levels (:levels app) :dirty? true))))

(defn sync-view
  "Bring font, layout context and layout up to date with the renderer's
  output and the text, then place the boxes. Each step is a cheap
  comparison unless its inputs changed. Between frames, as this is, it also
  trims the layout's cache. Without the time, `now`, a new width wraps
  the text at once, however long it is (see `sync-size`)."
  ([app] (sync-view app nil))
  ([app now]
   (let [app (-> app sync-fonts sync-theme (sync-size now) files/sync-modified)]
     (layout/trim! (:ctx app))
     (let [app (-> app sync-layout insets/sync-layouts)]
       (assoc (boxes/place-all app) :float-places (boxes/place-floats app))))))

(defn settle
  "Sync the view at time `now`, if given, then scroll as the last batch
  of events asked."
  ([app] (settle app nil))
  ([app now]
   (let [app (sync-view app now)]
     (scroll/clamp-scroll (if (:follow? app)
                            (-> app scroll/follow-caret (assoc :follow? false))
                            app)))))
