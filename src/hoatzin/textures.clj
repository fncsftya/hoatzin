(ns hoatzin.textures
  "Rasterized lines as SDL textures, cached by their text.

  A line's pixels depend only on its text and the font, not on where it sits
  or how its paragraph wrapped. So keying by text means typing re-rasterizes
  only the lines whose content changed, and a resize that merely moves lines
  rasterizes nothing. Clear the cache when the font or colour changes."
  (:require [jolt.ffi :as ffi]
            [hoatzin.coretext :as ct]
            [hoatzin.sdl :as sdl]))

(def ^:private spare
  "Off-screen textures kept, most recently drawn first, for scrolling back."
  256)

(defn cache [] (atom {:frame 0 :entries {}}))

(defn- upload [renderer line color]
  (let [{:keys [pixels width height pitch pad baseline]} (ct/rasterize-line line color)]
    (try
      (let [tex (sdl/check! (sdl/create-texture renderer sdl/PIXELFORMAT-RGBA32
                                                sdl/TEXTUREACCESS-STATIC width height)
                            "SDL_CreateTexture")]
        (sdl/check! (sdl/update-texture tex ffi/null pixels pitch) "SDL_UpdateTexture")
        (sdl/check! (sdl/set-texture-blend-mode tex sdl/BLENDMODE-BLEND-PREMULTIPLIED)
                    "SDL_SetTextureBlendMode")
        {:texture tex :width width :height height :pad pad :baseline baseline})
      (finally (ffi/free pixels)))))

(defn fetch!
  "The texture entry for `text`, rasterizing `line` (its CTLine) on a miss.
  Returns {:texture :width :height :pad :baseline}."
  [cache renderer text line color]
  (let [{:keys [frame entries]} @cache
        e (assoc (or (get entries text) (upload renderer line color)) :used frame)]
    (swap! cache assoc-in [:entries text] e)
    e))

(defn end-frame!
  "Evict all but this frame's textures and the `spare` most recent others."
  [cache]
  (let [{:keys [frame entries]} @cache]
    (when (> (count entries) spare)
      (let [stale (->> entries
                       (remove (fn [[_ e]] (= frame (:used e))))
                       (sort-by (fn [[_ e]] (- (:used e))))
                       (drop spare))]
        (doseq [[_ e] stale] (sdl/destroy-texture (:texture e)))
        (swap! cache update :entries #(apply dissoc % (map key stale)))))
    (swap! cache update :frame inc)))

(defn clear! [cache]
  (doseq [[_ e] (:entries @cache)] (sdl/destroy-texture (:texture e)))
  (swap! cache assoc :entries {}))
