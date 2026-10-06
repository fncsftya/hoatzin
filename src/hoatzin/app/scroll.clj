(ns hoatzin.app.scroll
  "Scrolling the text: :scroll is how far down it is, in pixels. The
  scroll bar shows it, and the wheel or dragging the bar's thumb moves it."
  (:require [hoatzin.app.geometry :as geo]
            [hoatzin.app.insets :as insets]
            [hoatzin.app.state :refer [px]]
            [hoatzin.lib.layout :as layout]))

(defn max-scroll [app] (max 0 (- (geo/content-height app) (geo/view-height app))))

(defn clamp-scroll [app]
  (update app :scroll #(max 0 (min % (max-scroll app)))))

(defn scroll-to [app scroll]
  (-> app (assoc :scroll (long (Math/round (double scroll))) :scroll-target nil)
      clamp-scroll (assoc :dirty? true)))

(defn follow-caret
  "Scroll just enough to bring the caret's line into view: in the inset
  it is in, if any, and that in the window."
  [app]
  (let [app (insets/follow app)
        L  (:layout app)
        lh (if (:inset app) (layout/line-height L) (geo/caret-line-height app))
        top (insets/caret-top app)
        vh (geo/view-height app)
        s  (:scroll app)]
    (assoc app :scroll-target nil
           :scroll (cond (< top s) top
                         (> (+ top lh) (+ s vh)) (- (+ top lh) vh)
                         :else s))))

;; ---------------------------------------------------------------- the scroll bar

(defn scrollbar
  "The scroll bar in render pixels, or nil when the text fits: the bar's
  column {:x :w}, the track {:top :height}, the thumb {:thumb-y :thumb-h},
  and the :max-scroll the track's travel stands for."
  [app]
  (let [ms (max-scroll app)]
    (when (pos? ms)
      (let [[w] (:size app)
            h       (geo/text-height app)
            inset   (px app 2)
            track   (- h (* 2 inset))
            thumb-h (min track (max (px app (:thumb-min app))
                                    (quot (* track (geo/view-height app))
                                          (geo/content-height app))))
            bw      (px app (:scrollbar-width app))]
        {:x (- w bw) :w bw :top inset :height track :thumb-h thumb-h
         :thumb-y (+ inset (long (Math/round (/ (* (- track thumb-h) (double (:scroll app))) ms))))
         :max-scroll ms}))))

(defn on-scrollbar? [app x]
  (when-let [{bx :x} (scrollbar app)] (>= x bx)))

;; ---------------------------------------------------------------- input

(defn on-wheel
  "The wheel moves where the text is headed; `glide` takes it there."
  [app now dy]
  (let [from   (or (:scroll-target app) (:scroll app))
        target (- from (* dy (:wheel-lines app) (layout/line-height (:layout app))))]
    (assoc app :scroll-target (max 0.0 (min (double target) (double (max-scroll app))))
           :scroll-at now)))

(defn glide
  "The text moved on towards where the wheel sent it, as of `now`: the
  distance left shrinks by e for every :scroll-glide-ms, and it arrives
  once within a pixel."
  [app now]
  (if-let [target (:scroll-target app)]
    (let [at   (or (:scroll-at app) now)
          pos  (double (or (:scroll-pos app) (:scroll app)))
          left (* (- pos target) (Math/exp (- (/ (- now at) (double (:scroll-glide-ms app))))))
          done (< (Math/abs left) 1.0)
          pos  (if done target (+ target left))]
      (cond-> (assoc app :scroll (long (Math/round pos)) :scroll-pos pos :scroll-at now :dirty? true)
        done (assoc :scroll-target nil :scroll-pos nil)
        true clamp-scroll))
    (dissoc app :scroll-pos)))

(defn gliding? [app] (some? (:scroll-target app)))

(defn on-scrollbar-click
  "Grab the thumb, or page towards the click on either side of it."
  [app y]
  (let [{:keys [thumb-y thumb-h]} (scrollbar app)
        lh   (layout/line-height (:layout app))
        page (max lh (- (geo/view-height app) lh))]
    (cond (< y thumb-y)              (scroll-to app (- (:scroll app) page))
          (>= y (+ thumb-y thumb-h)) (scroll-to app (+ (:scroll app) page))
          :else                      (assoc app :grab (- y thumb-y) :dirty? true))))

(defn on-thumb-drag [app y]
  (if-let [{:keys [top height thumb-h max-scroll]} (scrollbar app)]
    (let [travel (- height thumb-h)]
      (scroll-to app (if (pos? travel)
                       (* max-scroll (/ (- y (:grab app) top) (double travel)))
                       0)))
    app))

(defn hover
  "The pointer is `over?` the scroll bar, or not: the bar widens while it is."
  [app over?]
  (if (= over? (boolean (:hover? app)))
    app
    (assoc app :hover? over? :dirty? true)))
