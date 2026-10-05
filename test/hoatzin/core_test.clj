(ns hoatzin.core-test
  "The desktop host, run as its own process (see hoatzin.scripted-host)
  with SDL's dummy video driver: no window shows."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [jolt.process :as p]))

(def ^:private exit-ms
  "How long the host may take to start, run its script and exit. Well
  under the minute an idle thread pool would hold the process up for."
  30000)

(defn- run-host
  "Run the host on `script` until it exits: {:exit :out :err}, or nil if
  it is still running after `exit-ms`, which kills it."
  [script]
  (let [config (str (fs/create-temp-dir))      ; leave the user's settings be
        proc   (p/process ["jolt" "test/hoatzin/scripted_host.clj" script]
                          {:out :string :err :string
                           :extra-env {"SDL_VIDEO_DRIVER" "dummy"
                                       "XDG_CONFIG_HOME" config}})
        result (deref proc exit-ms nil)]
    (when-not result (p/destroy proc))
    (fs/delete-tree config)
    result))

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
