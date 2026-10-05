(ns hoatzin.ui-test
  (:require [clojure.test :refer [deftest is testing]]
            [hoatzin.ui :as ui]))

;; Text 7 px a character and 10 px high, at 1 px a point.
(def ctx {:scale 1.0 :text-size (fn [s _] [(* 7 (count s)) 10])})

(defn- rects
  "The placed rects, by node :id."
  [placed]
  (into {} (keep (fn [{:keys [node rect]}] (when-let [id (:id node)] [id rect]))) placed))

(defn- box [style & children] {:kind :box :style style :children (vec children)})
(defn- sized [id w h & {:as style}] {:kind :box :id id :style (merge {:width w :height h} style)})

(deftest measuring
  (testing "text, plus padding and border"
    (is (= [21 10] (ui/measure ctx {:kind :label :text "abc"})))
    (is (= [23 16] (ui/measure ctx {:kind :label :text "abc" :style {:padding [2 0] :border 1}})))
    (is (= [43 18] (ui/measure ctx {:kind :button :text "abc"})) "a button's own padding and border"))
  (testing "a box: its children along its direction, plus gaps"
    (is (= [30 55] (ui/measure ctx (box {:gap 5} (sized :a 30 20) (sized :b 10 30)))))
    (is (= [45 30] (ui/measure ctx (box {:direction :row :gap 5} (sized :a 30 20) (sized :b 10 30)))))
    (is (= [50 50] (ui/measure ctx (box {:padding 10} (sized :a 30 30)))))
    (is (= [8 9] (ui/measure ctx (box {:width 8 :height 9} (sized :a 30 30)))) "a set size wins"))
  (testing "absolute children take no room"
    (is (= [10 10] (ui/measure ctx (box {} (sized :a 10 10) (sized :b 99 99 :position :absolute)))))))

(deftest flow
  (testing "a column stretches its children across it"
    (is (= {:a [0 0 100 20] :b [0 25 100 30]}
           (rects (ui/place ctx (box {:gap 5} (sized :a nil 20) (sized :b nil 30)) [0 0 100 200])))))
  (testing "a row, aligned and justified"
    (is (= {:a [70 0 10 20] :b [90 10 10 10]}
           (rects (ui/place ctx (box {:direction :row :gap 10 :justify :end :align :start}
                                     (sized :a 10 20) (sized :b 10 10 :align-self :end))
                            [0 0 100 20]))))
    (is (= {:a [0 5 10 10] :b [45 5 10 10] :c [90 5 10 10]}
           (rects (ui/place ctx (box {:direction :row :justify :space-between :align :center}
                                     (sized :a 10 10) (sized :b 10 10) (sized :c 10 10))
                            [0 0 100 20]))))
    (is (= {:a [40 0 20 10]}
           (rects (ui/place ctx (box {:justify :center :align :center} (sized :a 20 10))
                            [0 0 100 10])))))
  (testing "growing children share what is left, by their :grow"
    (is (= {:a [0 0 10 10] :b [10 0 30 10] :c [40 0 60 10]}
           (rects (ui/place ctx (box {:direction :row}
                                     (sized :a 10 nil) (sized :b 0 nil :grow 1) (sized :c 0 nil :grow 2))
                            [0 0 100 10])))))
  (testing "inside the parent's padding and border"
    (is (= {:a [7 7 86 10]}
           (rects (ui/place ctx (box {:padding 5 :border 2} (sized :a nil 10)) [0 0 100 100])))))
  (testing "nested, offset by the outer rect"
    (is (= {:inner [10 20 50 10] :a [10 20 5 10] :b [15 20 5 10]}
           (rects (ui/place ctx (box {}
                                     (assoc (box {:direction :row :width 50} (sized :a 5 10) (sized :b 5 10))
                                            :id :inner))
                            [10 20 100 100]))))))

(deftest absolute
  (let [placed (fn [& kids] (rects (ui/place ctx (apply box {:padding 10 :border 2} kids) [0 0 200 100])))]
    (testing "from any edge, inside the border but not the padding"
      (is (= {:a [7 8 10 10]} (placed (sized :a 10 10 :position :absolute :left 5 :top 6))))
      (is (= {:a [183 82 10 10]} (placed (sized :a 10 10 :position :absolute :right 5 :bottom 6)))))
    (testing "stretched between opposite edges"
      (is (= {:a [12 2 176 96]} (placed (sized :a nil nil :position :absolute :left 10 :right 10
                                               :top 0 :bottom 0)))))
    (testing "painted after the flow, so on top"
      (is (= [:x :a] (keep (comp :id :node)
                           (ui/place ctx (box {} (sized :a 10 10 :position :absolute) (sized :x 5 5))
                                     [0 0 50 50])))))))

(deftest hitting
  (let [placed (ui/place ctx (box {}
                                  (sized :a nil 20)
                                  (sized :b 10 10 :position :absolute :left 0 :top 0))
                         [0 0 100 100])]
    (is (= :b (-> (ui/hit placed 5 5) :node :id)) "the topmost")
    (is (= :a (-> (ui/hit placed 50 5) :node :id)))
    (is (nil? (-> (ui/hit placed 50 50) :node :id)) "the outer box, which has no id")
    (is (nil? (ui/hit placed 150 50)))))
