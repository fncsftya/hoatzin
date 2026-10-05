(ns hoatzin.settings-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [hoatzin.settings :as settings]))

(deftest defaults
  (is (= {:editor-font {:family "Georgia" :size 20} :ui-font {:family "Menlo" :size 13}}
         settings/defaults)))

(deftest changing
  (is (= 24 (get-in (settings/change settings/defaults [:editor-font :size] 24)
                    [:editor-font :size])))
  (is (nil? (settings/change settings/defaults [:editor-font :size] 5)) "too small")
  (is (nil? (settings/change settings/defaults [:editor-font :size] 73)) "too big")
  (is (nil? (settings/change settings/defaults [:editor-font :size] 20.5)) "not whole")
  (is (nil? (settings/change settings/defaults [:ui-font :family] " ")) "no family")
  (is (nil? (settings/change settings/defaults [:theme] "dark")) "not a setting"))

(deftest json
  (testing "what the JSON has, where it is valid; defaults for the rest"
    (is (= settings/defaults (settings/read-json "{}")))
    (is (= {:editor-font {:family "Georgia" :size 18} :ui-font {:family "Menlo" :size 13}}
           (settings/read-json "{\"editor-font\": {\"size\": 18}}")))
    (is (= settings/defaults
           (settings/read-json "{\"editor-font\": {\"size\": \"big\"}, \"ui-font\": 3, \"x\": 1}")))
    (is (= settings/defaults (settings/read-json "[1, 2]"))))
  (testing "round trip"
    (let [s (settings/change settings/defaults [:ui-font :size] 15)]
      (is (= s (settings/read-json (settings/write-json s)))))))

(deftest the-file
  (let [dir (str (fs/create-temp-dir))
        path (str dir "/hoatzin/settings.json")]
    (try
      (is (= {:settings settings/defaults} (settings/read-file path)) "none: the defaults")
      (let [s (settings/change settings/defaults [:editor-font :size] 22)]
        (is (nil? (settings/write-file! path s)) "makes its directory")
        (is (= {:settings s} (settings/read-file path))))
      (spit path "{not json")
      (let [{:keys [settings error]} (settings/read-file path)]
        (is (= settings/defaults settings))
        (is (string? error)))
      (finally (fs/delete-tree dir)))))

(deftest the-file-is-in-the-xdg-config-home
  (is (re-find #"/hoatzin/settings\.json$" (settings/file))))
