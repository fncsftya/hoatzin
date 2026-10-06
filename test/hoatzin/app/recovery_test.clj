(ns hoatzin.app.recovery-test
  "Crash recovery end to end: what a session logs, kept on disk, read back
  after the session is gone, and the buffers it makes."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hoatzin.app.buffers :as buffers]
            [hoatzin.app.insets :as insets]
            [hoatzin.app.journal :as journal]
            [hoatzin.app.recovery :as recovery]
            [hoatzin.lib.sdl :as sdl]
            [hoatzin.test-support :as t :refer [with-session]]))

(def cmd sdl/KMOD-GUI)

(defmacro with-root
  "Run `body` with `root` a new, empty recovery directory."
  [[root] & body]
  `(let [~root (str (fs/create-temp-dir))]
     (try ~@body (finally (fs/delete-tree ~root)))))

(defn- open! [s path text] (t/send! s {:type :opened :path path :text text}))

(defn- crash!
  "What session `s` kept, in session directory `name` of `root`, as it
  stays when the editor dies, a pidless one."
  [root s name]
  (let [dir (str root "/" name)]
    (is (nil? (recovery/apply-ops! {:dir dir} (:journal @s))))
    dir))

(defn- recovered [root] (mapcat :buffers (recovery/sessions root)))

(deftest nothing-to-recover
  (with-root [root]
    (is (= [] (recovery/sessions root)) "no directory")
    (fs/create-dirs (str root "/empty"))
    (is (= [] (recovery/sessions root)))
    (is (not (fs/exists? (str root "/empty"))) "an empty session is cleared away")))

(deftest a-scratch-buffer-comes-back
  (with-root [root]
    (with-session [s]
      (t/type! s "hello\nworld")
      (t/press! s sdl/K-ESCAPE)
      (t/press! s sdl/K-LEFT)
      (t/send! s {:type :text :text "i"})
      (t/type! s "!")
      (crash! root s "1-1"))
    (let [[b :as bs] (recovered root)]
      (is (= 1 (count bs)))
      (is (= {:id 0 :text "hello\nworl!d" :caret 11 :insets []
              :meta {:path nil :name "scratch" :scratch? true :dir nil :mode nil}}
             b))
      (with-session [s2 :recovered bs]
        (is (= "hello\nworl!d" (t/text s2)))
        (is (= ["scratch" "scratch (recovered)"] (vals (sort (buffers/names (buffers/listing (t/app s2)))))))
        (is (str/starts-with? (:message (t/app s2)) "Recovered 1 unsaved buffer: scratch"))
        (testing "it is not the scratch buffer, and so is not lost to a quit"
          (is (not (:scratch? (t/app s2))))
          (is (journal/unsaved? (t/app s2))))
        (testing "and is kept again, at once, as a base"
          (is (= [:base] (map :op (:journal @s2))))
          (is (= "hello\nworl!d" (:text (first (:journal @s2))))))))))

(deftest a-file-buffer-comes-back-unsaved
  (with-root [root]
    (with-session [s :mode :normal]
      (open! s "/notes/a.txt" "one\ntwo\n")
      (t/type! s "i")
      (t/type! s "zero ")
      (t/press! s sdl/K-ESCAPE)
      (crash! root s "1-1"))
    (let [bs (recovered root)]
      (with-session [s2 :mode :normal :recovered bs :files {"/notes/a.txt" "one\ntwo\n"}]
        (is (= ["scratch" "a.txt"] (vals (sort (buffers/names (buffers/listing (t/app s2)))))))
        (is (= "/notes/a.txt" (:path (t/app s2))) "the file's buffer is the one shown")
        (is (= "zero one\ntwo\n" (t/text s2)))
        (is (:modified? (t/app s2)))
        (is (= "one\ntwo\n" (str (:saved (t/app s2)))) "what is saved is the file's")
        (is (buffers/unsaved? (select-keys (t/app s2) buffers/buffer-keys)))))))

(deftest a-file-already-as-it-was-needs-no-recovery
  (with-root [root]
    (with-session [s :mode :normal]
      (open! s "/notes/a.txt" "one\n")
      (t/type! s "i")
      (t/type! s "x")
      (crash! root s "1-1"))
    (with-session [s2 :mode :normal :recovered (recovered root)]
      ;; the file has, meanwhile, become what the edits made of it
      (is (= "xone\n" (t/text s2)))
      (is (= [] (filter #(= :discard (:op %)) (:journal @s2))))
      (t/command! s2 "w")
      (is (= {:op :discard :id 1} (last (:journal @s2)))))))

(deftest sections-come-back
  (with-root [root]
    (with-session [s :mode :normal]
      (open! s "/notes/n.auk" (pr-str {:content ["one" "two"]}))
      (t/press! s sdl/K-S cmd)
      (t/type! s "i")
      (t/type! s "a note")
      ;; the insets' changes wait to be kept: until the loop wakes for them
      (is (some? (journal/due-at (t/app s))))
      (t/advance! s journal/insets-delay-ms)
      (t/send! s {:type :tick})
      (is (nil? (journal/due-at (t/app s))))
      (crash! root s "1-1"))
    (let [[b] (recovered root)]
      (is (= [{:after 0 :text "a note" :insets []}] (:insets b)))
      (is (= "auk" (get-in b [:meta :mode])))
      (with-session [s2 :mode :normal :recovered [b]]
        (is (= "one\ntwo" (t/text s2)))
        (is (= [{:after 0 :text "a note" :insets []}] (insets/snapshot (t/app s2))))
        (is (= "auk" (:major-mode (t/app s2))))))))

(deftest headings-come-back
  (with-root [root]
    (with-session [s :mode :normal]
      (open! s "/notes/n.auk" (pr-str {:content ["one" "two"]}))
      (t/press! s sdl/K-DOWN)
      (t/type! s "2")
      (t/press! s sdl/K-S cmd)
      (t/type! s "i")
      (t/type! s "a note")
      (t/press! s sdl/K-ESCAPE)
      (t/type! s "1")
      (t/advance! s journal/insets-delay-ms)
      (t/send! s {:type :tick})
      (is (nil? (journal/due-at (t/app s))))
      (crash! root s "1-1"))
    (let [[b] (recovered root)]
      (is (= [0 2 0] (:levels b)))
      (is (= [1] (:levels (first (:insets b)))))
      (with-session [s2 :mode :normal :recovered [b]]
        (is (= [0 2 0] (insets/levels-of (t/app s2))))
        (is (= [1] (:levels (first (insets/snapshot (t/app s2))))))
        (is (:modified? (t/app s2)))))))

(deftest a-heading-alone-is-kept
  (with-session [s :mode :normal]
    (open! s "/notes/n.auk" (pr-str {:content ["one" "two"]}))
    (is (empty? (:journal @s)) "nothing to lose")
    (t/type! s "1")
    (t/send! s {:type :quit})
    (let [base (first (:journal @s))]
      (is (= :base (:op base)))
      (is (= [1 0] (:levels base))))))

(deftest quitting-keeps-the-insets-still-waiting
  (with-session [s :mode :normal]
    (open! s "/notes/n.auk" (pr-str {:content ["one"]}))
    (t/press! s sdl/K-S cmd)
    (t/type! s "i")
    (t/type! s "x")
    (t/send! s {:type :quit})
    (is (= {:op :insets :id 1 :gen 1 :insets [{:after 0 :text "x" :insets []}]}
           (last (:journal @s))))))

(deftest a-torn-or-stale-log
  (with-root [root]
    (let [dir (str root "/1-1")
          wal (str dir "/b0.wal")
          base {:op :base :id 0 :gen 1 :meta {:scratch? true :name "scratch"} :text "abc" :insets [] :caret 3}]
      (recovery/apply-ops! {:dir dir} [base {:op :edit :id 0 :gen 1 :lo 3 :old-count 0 :new "d" :caret 4}])
      (testing "a line half written is where reading stops"
        (spit wal "[:t 4 0 \"e\" 5]\n[:t 5 0 \"f" :append true)
        (is (= "abcde" (:text (first (recovered root))))))
      (testing "a log of another generation than the base's is ignored"
        (spit wal (str (pr-str [:gen 0]) "\n[:t 3 0 \"x\" 4]\n"))
        (is (= "abc" (:text (first (recovered root))))))
      (testing "an edit past the text is where reading stops, too"
        (spit wal (str (pr-str [:gen 1]) "\n[:t 3 0 \"x\" 4]\n[:t 99 0 \"y\" 5]\n[:t 4 0 \"z\" 5]\n"))
        (is (= "abcx" (:text (first (recovered root)))))))))

(deftest a-new-base-takes-the-log-place
  (with-root [root]
    (let [dir (str root "/1-1")
          meta {:scratch? true :name "scratch"}]
      (recovery/apply-ops! {:dir dir} [{:op :base :id 0 :gen 1 :meta meta :text "a" :insets [] :caret 1}
                                       {:op :edit :id 0 :gen 1 :lo 1 :old-count 0 :new "b" :caret 2}
                                       {:op :base :id 0 :gen 2 :meta meta :text "xyz" :insets [] :caret 3}
                                       {:op :edit :id 0 :gen 2 :lo 3 :old-count 0 :new "!" :caret 4}])
      (is (= "xyz!" (:text (first (recovered root)))))
      (recovery/apply-ops! {:dir dir} [{:op :discard :id 0}])
      (is (= [] (recovery/sessions root)) "discarded, there is nothing"))))

(deftest sessions-running-are-left-alone
  (with-root [root]
    (let [mine (recovery/start! root)]
      (is (nil? (recovery/apply-ops! mine [{:op :base :id 0 :gen 1 :meta {:scratch? true} :text "a"
                                            :insets [] :caret 1}])))
      (is (= [] (recovery/sessions root)) "this process is running")
      (spit (str (:dir mine) "/pid") "999999")
      (is (= 1 (count (recovery/sessions root))) "one whose process is not, is not")
      (recovery/finish! mine false)
      (is (not (fs/exists? (:dir mine)))))
    (testing "finishing with work to lose keeps it"
      (let [mine (recovery/start! root)]
        (recovery/apply-ops! mine [{:op :base :id 0 :gen 1 :meta {:scratch? true} :text "a" :insets [] :caret 1}])
        (recovery/finish! mine true)
        (is (fs/exists? (:dir mine)))))))

(deftest an-unreadable-session-is-kept-aside
  (with-root [root]
    (fs/create-dirs (str root "/1-1"))
    (spit (str root "/1-1/b0.base") "{:gen")
    (let [[session :as found] (recovery/sessions root)]
      (is (= 1 (count found)))
      (is (= [] (:buffers session)))
      (is (= 1 (count (:errors session))))
      (is (str/ends-with? (:dir session) ".unreadable"))
      (recovery/discard! session)
      (is (fs/exists? (:dir session)) "for the user to look at")
      (is (= [] (recovery/sessions root)) "and not found again"))))

(deftest unsaved-work-at-the-end
  (with-session [s]
    (t/type! s "scratch only")
    (is (not (journal/unsaved? (t/app s))) "the scratch buffer's changes are its own"))
  (with-session [s :mode :normal]
    (open! s "/notes/a.txt" "one")
    (is (not (journal/unsaved? (t/app s))))
    (t/type! s "i")
    (t/type! s "x")
    (is (journal/unsaved? (t/app s)))))
