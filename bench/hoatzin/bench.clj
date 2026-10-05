(ns hoatzin.bench
  "Editing benchmarks: a headless editor (as in the tests) on large
  generated documents, timing each step as one batch of the real event loop
  does it: handle the events, settle, draw.

    jolt -M:bench                    every scenario
    jolt -M:bench type move          the scenarios whose names contain a word
    jolt -M:bench --profile type     and where each step's time goes
    jolt -M:bench --gpu              with the app's GPU renderer

  The editor draws with the tests' software renderer, which is
  deterministic and keeps the numbers comparable from run to run. It has a
  stall of its own: now and then freeing a texture takes 15-200 ms, which
  shows as a step's max (but not in its gc column). --gpu draws with the
  renderer the app uses (Metal) into a hidden window instead, to check
  what is the software renderer's and what is ours. Presenting there waits
  for the display, vsync or not, so step times leave out the time spent in
  SDL_RenderPresent; the wait also slows the CPU work around it, so the
  medians run higher than they would in the app.

  Each line ends with the collections during the measured steps and their
  total time: a step's max is often one of them.

  --profile wraps the functions in `profiled` with timers and reports the
  time spent inside each, per step. Times nest: a function's time includes
  that of the profiled functions it calls."
  (:require [clojure.string :as str]
            [hoatzin.app :as app]
            [hoatzin.editor :as ed]
            [hoatzin.sdl :as sdl]
            [hoatzin.test-support :as t]
            [jolt.ffi :as ffi]))

;; ---------------------------------------------------------------- documents

(def ^:private words
  ["the" "hoatzin" "is" "a" "tropical" "bird" "found" "in" "swamps" "riparian"
   "forests" "and" "mangroves" "of" "Amazon" "Orinoco" "basins" "South" "America"
   "it" "notable" "for" "its" "chicks" "having" "claws" "on" "two" "wing" "digits"
   "leaves" "fermented" "crop" "like" "cow" "rumen" "smell" "stinkbird" "—" "café"])

(defn- prose
  "`n` lines of pseudo-random words, each about `width` characters, with a
  blank line every so often: deterministic, so runs compare."
  [n width]
  (let [sb (StringBuilder.)]
    (loop [i 0, seed 42]
      (if (= i n)
        (str sb)
        (let [[seed line]
              (loop [seed seed, len 0, acc []]
                (if (>= len width)
                  [seed (str/join " " acc)]
                  (let [seed (mod (+ (* seed 1103515245) 12345) 2147483648)
                        w (nth words (mod (quot seed 65536) (count words)))]
                    (recur seed (+ len 1 (count w)) (conj acc w)))))]
          (when (pos? i) (.append sb "\n"))
          (.append sb (if (= 6 (mod i 7)) "" line))
          (recur (inc i) seed))))))

;; ---------------------------------------------------------------- timing

(def ^:private profiled
  "The functions --profile times, by name; any that no longer exist are
  skipped."
  '[hoatzin.app/handle hoatzin.app/settle hoatzin.app/sync-view hoatzin.app/draw!
    hoatzin.app/normalize-newlines
    hoatzin.editor/insert hoatzin.editor/delete hoatzin.editor/word-range
    hoatzin.text/of hoatzin.text/replace hoatzin.text/lines hoatzin.text/changed-lines
    hoatzin.tree/build hoatzin.tree/splice
    hoatzin.layout/layout hoatzin.layout/paragraphs-of hoatzin.layout/rewrap
    hoatzin.layout/edit-range hoatzin.layout/trim!
    hoatzin.layout/caret hoatzin.layout/position-at hoatzin.layout/selection-segments
    hoatzin.layout/prev-position hoatzin.layout/next-position
    hoatzin.coretext/wrap hoatzin.coretext/rewrap hoatzin.coretext/make-line
    hoatzin.coretext/rasterize-line
    hoatzin.textures/fetch! hoatzin.textures/end-frame!
    hoatzin.sdl/create-texture hoatzin.sdl/update-texture hoatzin.sdl/destroy-texture])

(def ^:private profile (atom nil))

(defn- instrument! []
  (doseq [sym profiled
          :let [v (find-var sym)
                label (str sym)]
          :when v]
    (alter-var-root v (fn [f]
                        (fn [& args]
                          (if-let [p @profile]
                            (let [t0 (System/nanoTime)]
                              (try (apply f args)
                                   (finally
                                     (swap! p update label
                                            (fn [[n ns]] [(inc (or n 0)) (+ (or ns 0) (- (System/nanoTime) t0))])))))
                            (apply f args)))))))

(defn- ms [ns] (/ ns 1.0e6))

(defn- step!
  "One batch of the event loop: deliver `events`, settle, draw."
  [s events]
  (apply t/send! s events)
  (t/advance! s 1)
  (swap! s (fn [st] (assoc st :app (app/draw! (:app st) (:now st))))))

(def ^:private presenting
  "ns spent in SDL_RenderPresent, which `time-steps` leaves out."
  (atom 0))

(defn- time-present! []
  (alter-var-root #'sdl/render-present
                  (fn [f]
                    (fn [r]
                      (let [t0 (System/nanoTime)]
                        (try (f r) (finally (swap! presenting + (- (System/nanoTime) t0)))))))))

(defn- time-steps
  "Run `(events-fn i)` as a step `n` times; the time of each, in ns, less
  any spent presenting."
  [s n events-fn]
  (mapv (fn [i]
          (let [evs (events-fn i)
                p0 @presenting
                t0 (System/nanoTime)]
            (step! s evs)
            (- (System/nanoTime) t0 (- @presenting p0))))
        (range n)))

(defn- summary [times]
  (let [v (vec (sort times))
        n (count v)]
    {:n n
     :median (ms (nth v (quot n 2)))
     :p90 (ms (nth v (min (dec n) (long (* 0.9 n)))))
     :max (ms (peek v))}))

;; ---------------------------------------------------------------- scenarios

(defn- key-ev [k & [mod]] {:type :key :key k :mod (or mod 0)})
(defn- text-ev [s] {:type :text :text s})

(defn- open!
  "Load `text` as a file."
  [s text]
  (step! s [{:type :opened :path "/bench/big.txt" :text text}]))

(defn- to-middle!
  "Put the caret on the middle line, in insert mode."
  [s]
  (let [text (t/text s)
        mid (or (str/index-of text "\n" (quot (count text) 2)) 0)]
    (swap! s update :app #(assoc % :doc (ed/move (:doc %) (inc mid)) :follow? true))
    (step! s [(text-ev "i")])))

(defn- to-font-size!
  "Put the caret on the middle line, then open the settings window with
  the editor font size field focused."
  [s]
  (to-middle! s)
  (step! s [(key-ev sdl/K-ESCAPE)])
  (step! s (map (comp text-ev str) ":settings"))
  (step! s [(key-ev sdl/K-RETURN)])
  (let [{[x y w h] :rect} (some #(when (= :settings/editor-size (get-in % [:node :id])) %)
                                (:float-places (t/app s)))]
    (step! s [{:type :click :x (double (+ x (quot w 2))) :y (double (+ y (quot h 2)))}
              {:type :release}])))

(def ^:private docs
  {"lines"     (delay (prose 50000 70))   ; ~3.5 MB, 50k paragraphs
   "paragraph" (delay (prose 1 100000))}) ; one 100 KB paragraph

(def scenarios
  [{:name "open lines" :doc "lines" :n 3 :fresh? true
    :run (fn [_ text _] [{:type :opened :path "/bench/big.txt" :text text}])}
   {:name "type lines" :doc "lines" :n 100 :setup to-middle!
    :run (fn [_ _ i] [(text-ev (str (char (+ 97 (mod i 26)))))])}
   {:name "backspace lines" :doc "lines" :n 100 :setup to-middle!
    :run (fn [_ _ _] [(key-ev sdl/K-BACKSPACE)])}
   {:name "return lines" :doc "lines" :n 50 :setup to-middle!
    :run (fn [_ _ _] [(key-ev sdl/K-RETURN)])}
   {:name "move-right lines" :doc "lines" :n 100 :setup to-middle!
    :run (fn [_ _ _] [(key-ev sdl/K-RIGHT)])}
   {:name "move-down lines" :doc "lines" :n 100 :setup to-middle!
    :run (fn [_ _ _] [(key-ev sdl/K-DOWN)])}
   {:name "page-down lines" :doc "lines" :n 50 :setup to-middle!
    :run (fn [_ _ _] [(key-ev sdl/K-PAGEDOWN)])}
   {:name "scroll lines" :doc "lines" :n 100
    :run (fn [_ _ _] [{:type :wheel :dy -1}])}
   {:name "select-all lines" :doc "lines" :n 20
    :run (fn [_ _ i] [(key-ev (if (even? i) sdl/K-A sdl/K-LEFT) (if (even? i) sdl/KMOD-GUI 0))])}
   {:name "compose lines" :doc "lines" :n 50 :setup to-middle!
    :run (fn [_ _ i] [{:type :composition :text (if (even? i) "´" "") :cursor 1}])}
   {:name "type paragraph" :doc "paragraph" :n 20 :setup to-middle!
    :run (fn [_ _ _] [(text-ev "x")])}
   ;; the editor font size field, typed into and stepped, a new size each
   ;; step, faster than the font applies them
   {:name "type font-size lines" :doc "lines" :n 40 :setup to-font-size!
    :run (fn [_ _ i] [(key-ev sdl/K-BACKSPACE) (text-ev (str (+ 1 (mod i 2))))])}
   {:name "step font-size lines" :doc "lines" :n 40 :setup to-font-size!
    :run (fn [_ _ i] [(key-ev (if (even? i) sdl/K-UP sdl/K-DOWN))])}
   ;; and the font applying, once the size has been left alone
   {:name "apply font-size lines" :doc "lines" :n 20 :setup to-font-size!
    :run (fn [s _ i]
           (t/send! s (key-ev (if (even? i) sdl/K-UP sdl/K-DOWN)))
           (t/advance! s 1000)
           [{:type :tick}])}])

(def ^:private software? (atom true))

(def ^:private WINDOW-HIDDEN 0x8)

(ffi/defcfn ^:private set-render-vsync "SDL_SetRenderVSync" [:pointer :int] :bool)

(defn- gpu-session
  "A session (as hoatzin.test-support makes them) drawing with the default
  GPU renderer into a hidden window."
  []
  (let [[w r] (ffi/with-out [pw :pointer]
                (ffi/with-out [pr :pointer]
                  (sdl/check! (sdl/create-window-and-renderer
                               "bench" 800 600 (bit-or WINDOW-HIDDEN sdl/WINDOW-HIGH-PIXEL-DENSITY)
                               pw pr)
                              "SDL_CreateWindowAndRenderer")
                  [(ffi/read pw :pointer) (ffi/read pr :pointer)]))]
    ;; time the work, not the wait for the display
    (sdl/check! (set-render-vsync r 0) "SDL_SetRenderVSync")
    (atom {:app (app/create {:renderer r
                             :density-fn #(sdl/get-window-pixel-density w)
                             :mode :insert
                             :now 0})
           :now 0 :window w :renderer r})))

(defn- session []
  (if @software?
    (t/session :width 800 :height 600 :density 2.0)
    (gpu-session)))

(defn- close! [s]
  (if-let [w (:window @s)]
    (do (app/destroy! (:app @s))
        (sdl/destroy-renderer (:renderer @s))
        (sdl/destroy-window w))
    (t/close! s)))

(defn- measure
  "Time `n` steps of scenario `run` in session `s`; fresh scenarios get a
  new session for each."
  [s n run text]
  (if s
    (time-steps s n #(run s text %))
    (vec (for [i (range n)]
           (let [s (session)]
             (try (first (time-steps s 1 (fn [_] (run s text i))))
                  (finally (close! s))))))))

(defn- run-scenario [{:keys [name doc n setup run fresh?]} profile?]
  (let [text @(docs doc)
        s (when-not fresh? (session))]
    (try
      (when s
        (open! s text)
        (when setup (setup s))
        (step! s (run s text -1)))          ; one step to warm up
      (when profile? (reset! profile (atom {})))
      (let [gc0 (jolt.host/gc-count), gcns0 (jolt.host/gc-cpu-nanos)
            times (measure s n run text)
            p (some-> @profile deref)]
        (reset! profile nil)
        (assoc (summary times) :name name :profile p
               :gcs (- (jolt.host/gc-count) gc0)
               :gc-ms (ms (- (jolt.host/gc-cpu-nanos) gcns0))))
      (finally (some-> s close!)))))

(defn- report [{:keys [name n median p90 max profile gcs gc-ms]}]
  (println (format "%-18s %4d steps  median %9.3f ms  p90 %9.3f ms  max %9.3f ms  gc %3d %8.1f ms"
                   name n median p90 max gcs gc-ms))
  (when profile
    (doseq [[label [calls ns]] (sort-by (comp - second val) profile)]
      (println (format "    %-32s %9.3f ms/step %8.1f calls/step"
                       label (/ (ms ns) n) (/ calls (double n)))))))

(defn -main [& args]
  (let [profile? (some #{"--profile"} args)
        filters (remove #{"--profile" "--gpu"} args)
        chosen (filter (fn [{:keys [name]}]
                         (or (empty? filters) (some #(str/includes? name %) filters)))
                       scenarios)]
    (reset! software? (not (some #{"--gpu"} args)))
    (when-not @software?
      (sdl/check! (sdl/init sdl/INIT-VIDEO) "SDL_Init")
      (time-present!))
    (when profile? (instrument!))
    (try
      (doseq [sc chosen]
        (report (run-scenario sc profile?))
        (flush))
      (finally
        (when-not @software? (sdl/quit))))))
