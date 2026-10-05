(ns hoatzin.test-support
  "Headless test harness.

  An editor session renders with SDL's software renderer into an offscreen
  target texture: no window, no OS event loop, no clock. Input is synthetic
  data events and time only moves when a test advances it, so every run
  produces the same pixels.

  Golden images live in test/golden/<name>.png. A mismatch writes
  target/golden/<name>-actual.png and <name>-diff.png (differing pixels in
  red over a dimmed copy of the golden). Regenerate goldens with
  UPDATE_GOLDEN=1 after checking the change is intended."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [is]]
            [jolt.ffi :as ffi]
            [hoatzin.app :as app]
            [hoatzin.lib.editor :as ed]
            [hoatzin.lib.sdl :as sdl]))

;; ---------------------------------------------------------------- canvas

(defn- resize-canvas [{:keys [renderer target] :as canvas} w h]
  (let [t (sdl/check! (sdl/create-texture renderer sdl/PIXELFORMAT-RGBA32
                                          sdl/TEXTUREACCESS-TARGET w h)
                      "SDL_CreateTexture")]
    (sdl/check! (sdl/set-render-target renderer t) "SDL_SetRenderTarget")
    (some-> target sdl/destroy-texture)
    (assoc canvas :target t)))

(defn- canvas
  "A software renderer drawing into a `w` x `h` pixel target texture."
  [w h]
  (let [surface  (sdl/check! (sdl/create-surface 1 1 sdl/PIXELFORMAT-RGBA32) "SDL_CreateSurface")
        renderer (sdl/check! (sdl/create-software-renderer surface) "SDL_CreateSoftwareRenderer")]
    (resize-canvas {:surface surface :renderer renderer} w h)))

(defn- destroy-canvas! [{:keys [surface renderer target]}]
  (sdl/set-render-target renderer ffi/null)
  (sdl/destroy-texture target)
  (sdl/destroy-renderer renderer)
  (sdl/destroy-surface surface))

;; ---------------------------------------------------------------- images

(defn- surface->image
  "Copy any SDL surface into {:width :height :pixels} (tightly packed RGBA)."
  [surface]
  (let [rgba (sdl/check! (sdl/convert-surface surface sdl/PIXELFORMAT-RGBA32) "SDL_ConvertSurface")]
    (try
      (let [w     (ffi/read rgba :int sdl/O-surface-w)
            h     (ffi/read rgba :int sdl/O-surface-h)
            pitch (ffi/read rgba :int sdl/O-surface-pitch)
            px    (ffi/read rgba :pointer sdl/O-surface-pixels)
            row   (* 4 w)]
        {:width w :height h
         :pixels (if (= pitch row)
                   (ffi/read-array px (* row h))
                   (ffi/with-alloc [packed (* row h)]
                     (doseq [y (range h)]
                       (ffi/copy (+ px (* y pitch)) (+ packed (* y row)) row))
                     (ffi/read-array packed (* row h))))})
      (finally (sdl/destroy-surface rgba)))))

(defn read-png [path]
  (let [s (sdl/check! (sdl/load-png (str path)) (str "SDL_LoadPNG " path))]
    (try (surface->image s) (finally (sdl/destroy-surface s)))))

(defn write-png! [{:keys [width height pixels]} path]
  (fs/create-dirs (fs/parent path))
  (ffi/with-alloc [buf (alength pixels)]
    (ffi/write-array buf pixels)
    (let [s (sdl/check! (sdl/create-surface-from width height sdl/PIXELFORMAT-RGBA32 buf (* 4 width))
                        "SDL_CreateSurfaceFrom")]
      (try (sdl/check! (sdl/save-png s (str path)) (str "SDL_SavePNG " path))
           (finally (sdl/destroy-surface s))))))

(defn diff-images
  "Compare RGBA images. A pixel differs when any channel differs by more
  than `tolerance`. Returns {:differing n :max-delta d :diff image}, or
  {:size-mismatch [[w h] [w h]]}."
  [expected actual tolerance]
  (let [{w :width h :height a :pixels} expected
        {b :pixels} actual]
    (if (not= [w h] [(:width actual) (:height actual)])
      {:size-mismatch [[w h] [(:width actual) (:height actual)]]}
      (let [n   (* w h)
            out (byte-array (* 4 n))]
        (loop [p 0, differing 0, max-delta 0]
          (if (= p n)
            {:differing differing :max-delta max-delta
             :diff {:width w :height h :pixels out}}
            (let [i (* 4 p)
                  d (loop [c 0, m 0]
                      (if (= c 4)
                        m
                        (recur (inc c)
                               (max m (Math/abs (- (bit-and (aget a (+ i c)) 0xff)
                                                   (bit-and (aget b (+ i c)) 0xff)))))))
                  bad? (> d tolerance)]
              ;; red where different; elsewhere the expected pixel, dimmed
              (dotimes [c 3]
                (aset out (+ i c)
                      (unchecked-byte (if bad?
                                        (if (zero? c) 255 0)
                                        (quot (bit-and (aget a (+ i c)) 0xff) 4)))))
              (aset out (+ i 3) (unchecked-byte 255))
              (recur (inc p) (if bad? (inc differing) differing) (max max-delta d)))))))))

(def golden-dir "test/golden")
(def output-dir "target/golden")

(defn- update-goldens? [] (= "1" (System/getenv "UPDATE_GOLDEN")))

(defn matches-golden?
  "Assert (with clojure.test/is) that `image` matches golden `name`.

  The software renderer is deterministic, so by default no pixel may differ.
  CoreText's antialiasing can shift slightly between macOS releases; loosen
  :tolerance (per channel, 0-255) or :max-differing (pixel count) for a
  test that must survive that, or regenerate the goldens."
  [name image & {:keys [tolerance max-differing] :or {tolerance 0 max-differing 0}}]
  (let [golden (fs/path golden-dir (str name ".png"))
        actual (fs/path output-dir (str name "-actual.png"))
        diff   (fs/path output-dir (str name "-diff.png"))]
    (cond
      (update-goldens?)
      (do (write-png! image golden)
          (is true))

      (not (fs/exists? golden))
      (do (write-png! image actual)
          (is false (str "no golden image " golden "; the render is at " actual
                         ". If it looks right, run with UPDATE_GOLDEN=1.")))

      :else
      (let [{:keys [differing max-delta size-mismatch] d :diff}
            (diff-images (read-png golden) image tolerance)
            ok? (and (nil? size-mismatch) (<= differing max-differing))]
        (if ok?
          (do (fs/delete-if-exists actual) (fs/delete-if-exists diff))
          (do (write-png! image actual)
              (when d (write-png! d diff))))
        (is ok? (if size-mismatch
                  (str name ": size " (second size-mismatch) ", golden " (first size-mismatch)
                       "; see " actual)
                  (str name ": " differing " pixels differ (max channel delta " max-delta
                       "); see " actual " and " diff)))))))

;; ---------------------------------------------------------------- sessions

(def font-families
  "The fonts a session finds installed: some of those every macOS has, so
  that renders don't depend on what else is."
  ["American Typewriter" "Arial" "Avenir" "Baskerville" "Courier New" "Futura"
   "Georgia" "Gill Sans" "Helvetica" "Menlo" "Optima" "Palatino" "Times New Roman"
   "Trebuchet MS" "Verdana"])

(defn session
  "A headless editor, `width` x `height` points at `density`, starting in
  `mode`: :insert unless given, so tests can type straight away, and nil
  for the editor's own default. A mutable
  map in an atom: {:app :canvas :now :clipboard :dialogs :open-dialogs
  :save-dialogs :dir-dialogs :files :write-error :saved-settings
  :settings-error}; the editor reads and writes :clipboard, counts the
  open dialogs it shows in :dialogs, records the paths the open, save and
  directory dialogs it shows start at in :open-dialogs, :save-dialogs and
  :dir-dialogs, reads from :files, a map of path to text, and writes into
  it, unless there is a :write-error to fail with. It saves its settings
  into :saved-settings, unless there is a :settings-error to fail with.
  The minor modes' data it is asked for is noted in :data-asked, as
  [mode file], to be answered with `data-read!`, and what it is given to
  keep is kept in :mode-data, by [mode file].
  `dir` is its working directory, nil unless given, and `mode-sources`
  modes besides the editor's own (see hoatzin.app/create).
  Close with `close!`."
  [& {:keys [width height density clipboard mode dir mode-sources]
      :or   {width 400 height 300 density 2.0 clipboard "" mode :insert}}]
  (let [c (canvas (long (* width density)) (long (* height density)))
        s (atom {:canvas c :now 0 :clipboard clipboard :density density :dialogs 0
                 :open-dialogs [] :save-dialogs [] :dir-dialogs [] :files {}
                 :data-asked [] :mode-data {}})]
    (swap! s assoc :app (app/create (cond-> {:renderer     (:renderer c)
                                             :density-fn   (constantly (double density))
                                             :clipboard-fn #(:clipboard @s)
                                             :set-clipboard-fn #(swap! s assoc :clipboard %)
                                             :open-dialog-fn #(swap! s (fn [st] (-> st (update :dialogs inc)
                                                                                    (update :open-dialogs conj %))))
                                             :save-dialog-fn #(swap! s update :save-dialogs conj %)
                                             :dir-dialog-fn #(swap! s update :dir-dialogs conj %)
                                             :read-file-fn #(if-let [text (get-in @s [:files %])]
                                                              {:text text}
                                                              {:error "No such file"})
                                             :dir          dir
                                             :write-file-fn
                                             (fn [path text]
                                               (or (:write-error @s)
                                                   (do (swap! s assoc-in [:files path] text) nil)))
                                             :save-settings-fn
                                             (fn [settings]
                                               (or (:settings-error @s)
                                                   (do (swap! s assoc :saved-settings settings) nil)))
                                             :font-families-fn (constantly font-families)
                                             :load-mode-data-fn #(swap! s update :data-asked conj [%1 %2])
                                             :save-mode-data-fn
                                             (fn [mode file data]
                                               (swap! s (fn [st] (if (nil? data)
                                                                   (update st :mode-data dissoc [mode file])
                                                                   (assoc-in st [:mode-data [mode file]] data))))
                                               nil)
                                             :now          0}
                                      mode (assoc :mode mode)
                                      mode-sources (assoc :mode-sources mode-sources))))
    s))

(defn close! [s]
  (app/destroy! (:app @s))
  (destroy-canvas! (:canvas @s)))

(defmacro with-session
  "Run `body` with `sym` bound to a new session, closing it afterwards."
  [[sym & opts] & body]
  `(let [~sym (session ~@opts)]
     (try ~@body (finally (close! ~sym)))))

(defn app [s] (:app @s))
(defn doc
  "The document, with its text as a string to compare."
  [s]
  (update (:doc (app s)) :text str))
(defn text [s] (str (:text (:doc (app s)))))
(defn caret [s] (:caret (:doc (app s))))
(defn selected [s] (ed/selected-text (:doc (app s))))

(defn send!
  "Deliver events at the session's current time, then settle the view, as
  one batch of the real event loop does."
  [s & events]
  ;; Not inside swap!: handling may write the clipboard, which is in `s` too.
  (let [{:keys [now app]} @s]
    (swap! s assoc :app (app/settle (reduce #(app/handle %1 %2 now) app events))))
  s)

(defn type!
  "Type `text` one character (code point) at a time, as keystrokes arrive."
  [s text]
  (apply send! s (map (fn [c] {:type :text :text (str c)}) text)))

(defn press!
  ([s key] (press! s key 0))
  ([s key mod] (send! s {:type :key :key key :mod mod})))

(defn click!
  "A left click; :clicks 2 for a double click."
  [s x y & {:keys [mod clicks] :or {mod 0 clicks 1}}]
  (send! s {:type :click :x x :y y :mod mod :clicks clicks} {:type :release}))

(defn double-click!
  "A double click: the first click, then the second, as SDL reports them."
  [s x y]
  (click! s x y)
  (click! s x y :clicks 2))

(defn drag!
  "Press at the first point, move through the rest, and release."
  [s [x y] & points]
  (apply send! s (concat [{:type :click :x x :y y}]
                         (map (fn [[x y]] {:type :drag :x x :y y}) points)
                         [{:type :release}])))

(defn compose!
  "The input method's composition (marked text), as SDL reports it."
  ([s text] (compose! s text (count text)))
  ([s text cursor] (send! s {:type :composition :text text :cursor cursor})))

(defn dead-key!
  "A dead key and the key after it, as macOS reports them: composition of
  `accent`, composition cleared, then `result` committed.
  e.g. (dead-key! s \"´\" \"é\") for option-e then e."
  [s accent result]
  (compose! s accent 1)
  (send! s {:type :composition :text "" :cursor 0} {:type :text :text result}))

(defn advance!
  "Move the session's clock forward `ms`."
  [s ms]
  (swap! s update :now + ms)
  s)

(defn data-read!
  "The host's answer to the editor asking for minor mode `mode`'s file
  `file`: what it kept there, if anything."
  [s mode file]
  (send! s {:type :mode-data :mode mode :file file :data (get-in @s [:mode-data [mode file]])}))

(defn set-clipboard! [s text] (swap! s assoc :clipboard text) s)

(defn command!
  "Type `:` and `command` in normal mode, then return."
  [s command]
  (type! s (str ":" command))
  (press! s sdl/K-RETURN))

(defn resize!
  "Resize the offscreen canvas to `width` x `height` points."
  [s width height]
  (swap! s (fn [{:keys [canvas density] :as st}]
             (let [st (assoc st :canvas (resize-canvas canvas (long (* width density))
                                                       (long (* height density))))]
               (assoc st :app (app/settle (:app st))))))
  s)

(defn render!
  "Draw the editor at the session's current time and return the image."
  [s]
  (swap! s (fn [st] (assoc st :app (app/draw! (:app st) (:now st)))))
  (let [r (get-in @s [:canvas :renderer])
        surface (sdl/check! (sdl/render-read-pixels r ffi/null) "SDL_RenderReadPixels")]
    (try (surface->image surface) (finally (sdl/destroy-surface surface)))))
