(ns hoatzin.lib.coretext-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hoatzin.lib.coretext :as ct]))

(deftest font-families
  (let [families (ct/font-families)]
    (is (some #{"Georgia"} families))
    (is (some #{"Menlo"} families))
    (is (= families (sort-by (juxt str/lower-case identity) families)) "alphabetically")
    (is (= families (distinct families)))
    (is (not-any? #(str/starts-with? % ".") families) "none of the hidden ones")))
