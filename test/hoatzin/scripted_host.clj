(ns hoatzin.scripted-host
  "The desktop host (hoatzin.core/-main), fed a script of app events in
  place of the user's input. core_test runs it as its own process, to see
  how the real host ends:

    jolt test/hoatzin/scripted_host.clj SCRIPT

  Each time the host waits for SDL events, the next event of the script is
  queued and an SDL user event pushed to wake it, which `decode` answers
  with the queued event. The script then runs at the loop's own pace, with
  no thread of its own to keep the process up."
  (:require [hoatzin.core :as core]
            [hoatzin.lib.sdl :as sdl]
            [jolt.ffi :as ffi]))

(def ^:private big-text
  "Enough text that laying it out wraps its paragraphs on several threads."
  (apply str (repeat 5000 "The hoatzin is a tropical bird of the Amazon basin.\n")))

(def ^:private colon-q
  [{:type :text :text ":"} {:type :text :text "q"} {:type :key :key sdl/K-RETURN :mod 0}])

(def ^:private typed
  "Insert mode, and some text typed: then nothing more, the host waits on."
  (into [{:type :text :text "i"}]
        (map (fn [c] {:type :text :text (str c)}) "unsaved work")))

(def scripts
  {"close"     [{:type :quit}]
   "typed"     typed
   "typed-close" (conj typed {:type :quit})
   "colon-q"   colon-q
   "big-close" [{:type :opened :path "/tmp/big.txt" :text big-text} {:type :quit}]
   "big-colon-q" (into [{:type :opened :path "/tmp/big.txt" :text big-text}] colon-q)})

(defn- feed!
  "Make the host's event loop take `events`, in order, as its input."
  [events]
  (let [pending (atom (seq events))
        queued  (atom clojure.lang.PersistentQueue/EMPTY)]
    (alter-var-root #'sdl/wait-event-timeout
                    (fn [wait]
                      (fn [ev ms]
                        (when-let [e (first @pending)]
                          (swap! pending next)
                          (swap! queued conj e)
                          (ffi/with-alloc [u sdl/EVENT-SIZE]
                            (ffi/write u :uint sdl/EVENT-USER sdl/O-event-type)
                            (sdl/push-event u)))
                        (wait ev ms))))
    (alter-var-root #'core/decode
                    (fn [decode]
                      (fn [renderer dialogs ev]
                        (if-let [e (peek @queued)]
                          (do (swap! queued pop) e)
                          (decode renderer dialogs ev)))))))

(let [[script] *command-line-args*]
  (feed! (or (scripts script) (throw (ex-info "no such script" {:script script}))))
  (core/-main)
  (println "main returned"))
