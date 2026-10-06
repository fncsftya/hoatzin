(ns hoatzin.core-test
  "The desktop host, run as its own process (see hoatzin.scripted-host)
  with SDL's dummy video driver: no window shows."
  (:require [babashka.fs :as fs]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jolt.process :as p]))

(def ^:private exit-ms
  "How long the host may take to start, run its script and exit. Well
  under the minute an idle thread pool would hold the process up for."
  30000)

(defn- host
  "Start the host on `script`, with `config` as its XDG config home."
  [script config]
  (p/process ["jolt" "test/hoatzin/scripted_host.clj" script]
             {:out :string :err :string
              :extra-env {"SDL_VIDEO_DRIVER" "dummy"
                          "XDG_CONFIG_HOME" config}}))

(defn- run-host
  "Run the host on `script` until it exits: {:exit :out :err}, or nil if
  it is still running after `exit-ms`, which kills it. It has `config`, a
  new directory unless given, as its XDG config home: that leaves the
  user's settings, and their recovery files, be."
  ([script] (let [config (str (fs/create-temp-dir))]
              (try (run-host script config)
                   (finally (fs/delete-tree config)))))
  ([script config]
   (let [proc   (host script config)
         result (deref proc exit-ms nil)]
     (when-not result (p/destroy proc))
     result)))

(defn- exits? [script]
  (let [{:keys [exit out err] :as result} (run-host script)]
    (is (some? result) (str script ": still running " exit-ms " ms on"))
    (when result
      (is (= 0 exit) (str script ": " err))
      (is (re-find #"main returned" out)))))

(deftest ^:integration quitting-exits
  (testing "closing the window"
    (exits? "close"))
  (testing ":q"
    (exits? "colon-q"))
  ;; Laying out a text this big wraps it on a pool of threads, whose idle
  ;; workers would keep the process alive once the host is done.
  (testing "after laying out a big text"
    (exits? "big-close")
    (exits? "big-colon-q")))

;; ---------------------------------------------------------------- crashes

(defn- session-files
  "The recovery files `config` has, as {name text}."
  [config]
  (into {} (map (juxt #(str (fs/relativize config %)) #(slurp (str %))))
        (filter fs/regular-file? (fs/glob (str config "/hoatzin/recovery") "*/b*"))))

(defn- wait-for
  "Whether `pred` holds within `exit-ms`, polled."
  [pred]
  (let [until (+ (System/currentTimeMillis) exit-ms)]
    (loop []
      (cond (pred) true
            (> (System/currentTimeMillis) until) false
            :else (do (Thread/sleep 100) (recur))))))

(deftest ^:integration a-killed-host-is-recovered
  (let [config (str (fs/create-temp-dir))]
    (try
      (let [proc (host "typed" config)
            kept? #(some (fn [[n s]] (and (str/ends-with? n ".wal") (str/includes? s "\"k\"")))
                         (session-files config))]
        (testing "the work is logged as it is typed"
          (is (wait-for kept?)))
        (testing "killed, with no chance to save or tidy"
          (is (zero? (:exit (shell/sh "kill" "-9" (str (.pid (:proc proc))))))))
        @proc
        (let [killed (session-files config)]
          (is (seq killed))
          (testing "starting again recovers it, and keeps it in a session of its own"
            (let [{:keys [exit err]} (run-host "close" config)]
              (is (= 0 exit) err))
            (let [now (session-files config)]
              (is (= 1 (count (filter #(str/ends-with? (key %) ".base") now))))
              (is (not-any? #(contains? now %) (keys killed)) "the old session is gone")
              (is (some #(str/includes? (val %) "unsaved work") now) "with what was typed")))))
      (finally (fs/delete-tree config))))
  (testing "quitting with only the scratch buffer's changes keeps nothing"
    (let [config (str (fs/create-temp-dir))]
      (try (is (= 0 (:exit (run-host "typed-close" config))))
           (is (empty? (session-files config)))
           (finally (fs/delete-tree config))))))
