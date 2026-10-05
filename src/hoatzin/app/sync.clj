(ns hoatzin.app.sync
  "Bringing what the app shows up to date with what it holds: the fonts
  and colours with the settings and the density, the layout with the text and the
  window's size, and the boxes' places with both."
  (:require [hoatzin.app.boxes :as boxes]
            [hoatzin.app.display :as display]
            [hoatzin.app.face :as face]
            [hoatzin.app.files :as files]
            [hoatzin.app.scroll :as scroll]
            [hoatzin.app.state :refer [px]]
            [hoatzin.app.theme :as theme]
            [hoatzin.lib.coretext :as ct]
            [hoatzin.lib.layout :as layout]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.lib.textures :as textures]))

(defn- release-editor-view! [{:keys [ctx textures font]}]
  (some-> ctx layout/release-context)
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
                             :ctx nil :dirty? true)
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
  "Take the renderer's output size, and wrap the text to fit it."
  [app]
  (let [[w _ :as size] (sdl/render-output-size (:renderer app))
        wrap (max 1 (- w (* 2 (px app (:margin app)))))
        app (if (= wrap (get-in app [:ctx :width]))
              app
              (do (some-> (:ctx app) layout/release-context)
                  ;; Re-wrapping moves every line; keep the caret's in view.
                  (assoc app :ctx (layout/context (:font app) wrap) :layout nil :follow? true)))]
    (if (= size (:size app)) app (assoc app :size size :dirty? true))))

(defn- sync-layout
  "Lay the text out again, if it has changed since it last was."
  [app]
  (let [shown (display/display-key app)]
    (if (and (:layout app) (display/same-display? shown (:laid-out app)))
      app
      (assoc app :layout (layout/layout (:ctx app) (display/display-text app))
                 :laid-out shown :dirty? true))))

(defn sync-view
  "Bring font, layout context and layout up to date with the renderer's
  output and the text, then place the boxes. Each step is a cheap
  comparison unless its inputs changed. Between frames, as this is, it also
  trims the layout's cache."
  [app]
  (let [app (-> app sync-fonts sync-theme sync-size files/sync-modified)]
    (layout/trim! (:ctx app))
    (let [app (sync-layout app)]
      (assoc app :block-places (boxes/place-blocks app) :float-places (boxes/place-floats app)))))

(defn settle
  "Sync the view, then scroll as the last batch of events asked."
  [app]
  (let [app (sync-view app)]
    (scroll/clamp-scroll (if (:follow? app)
                           (-> app scroll/follow-caret (assoc :follow? false))
                           app))))
