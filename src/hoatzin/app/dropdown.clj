(ns hoatzin.app.dropdown
  "A dropdown's list (see hoatzin.lib.ui): opened from the dropdown with
  the focus, it shows that dropdown's options just below it (or above, if
  there is more room there), no more than :list-rows of them at once, and
  scrolls through the rest.

  While it is open it takes the input. It is on one option at a time,
  which is highlighted: the one under the pointer, or the one the keys
  move it to. Up and down move by one, page up and page down by a list's
  worth less one, and home and end go to the first and last. Typing the
  start of an option's name goes to it, ignoring case; letters typed
  within :type-ahead-ms of each other spell out one name. Return, a
  space (unless it is part of a name being typed) or a click on an option
  chooses it, and closes the list. Escape, or a click anywhere else,
  closes it without choosing; tab closes it as the focus moves on.

  The wheel scrolls it, and so do the keys, to show the option they move
  to. It doesn't jump: it glides where it is headed, quickly at first and
  slowing as it arrives (see `glide`).

  The app's :list is the open list, or nil:
    {:id id            the dropdown's
     :active i         the option it is on, or nil
     :scroll px        how far down it is scrolled, in render pixels; a
     :target px        double, gliding towards :target
     :at ms            when it was last moved on towards it
     :pointer [x y]    where the pointer is over it, while that leads
     :typed s          the name being typed, and when it last was
     :typed-at ms}"
  (:require [clojure.string :as str]
            [hoatzin.app.boxes :refer [focused-field hover ui-value]]
            [hoatzin.app.geometry :refer [text-height]]
            [hoatzin.app.settings-window :as settings-window]
            [hoatzin.app.state :refer [px]]
            [hoatzin.lib.sdl :as sdl]))

(defn- clamp [v lo hi] (max lo (min hi v)))

(defn- in-rect? [[rx ry rw rh] x y]
  (and (<= rx x) (< x (+ rx rw)) (<= ry y) (< y (+ ry rh))))

(defn- dropdown
  "The dropdown with the focus, placed, or nil."
  [app]
  (when-let [{:keys [node] :as placed} (focused-field app)]
    (when (= :dropdown (:kind node)) placed)))

;; ---------------------------------------------------------------- where

(defn row-height
  "How tall an option's row is, in render pixels."
  [app]
  (+ (get-in app [:ui :metrics :line-height]) (* 2 (px app (:list-padding app)))))

(defn place
  "Where the open list is, in render pixels, or nil: {:rect [x y w h]
  :inner rect, inside its border, where the rows are :row-h :rows, how
  many show :options :max-scroll}. As wide as its dropdown, below it,
  unless there is more room above, and no taller than the room there is,
  down to the status bar."
  [app]
  (when (:list app)
    (when-let [{:keys [node] [x y w h] :rect} (dropdown app)]
      (let [options (vec (:options node))
            rh    (row-height app)
            bw    (px app 1)
            gap   (px app 2)
            most  (min (count options) (:list-rows app))
            below (- (text-height app) (+ y h gap))
            above (- y gap)
            fits  (fn [room] (quot (- room bw bw) rh))
            down? (or (>= (fits below) most) (>= below above))
            rows  (max 1 (min most (fits (if down? below above))))
            ph    (+ (* rows rh) bw bw)
            py    (if down? (+ y h gap) (- y gap ph))]
        {:rect [x py w ph] :inner [(+ x bw) (+ py bw) (max 0 (- w bw bw)) (* rows rh)]
         :row-h rh :rows rows :options options
         :max-scroll (* rh (max 0 (- (count options) rows)))}))))

(defn- row-at
  "The option under render pixel (x, y), or nil."
  [app x y]
  (let [{:keys [inner row-h options]} (place app)]
    (when (in-rect? inner x y)
      (let [i (long (Math/floor (/ (+ (- y (second inner)) (get-in app [:list :scroll])) row-h)))]
        (when (< -1 i (count options)) i)))))

;; ---------------------------------------------------------------- scrolling

(defn- head-for
  "Have the list glide to scroll `target`, kept within it."
  [app target]
  (assoc-in app [:list :target] (double (clamp target 0 (:max-scroll (place app))))))

(defn- reveal
  "Have the list glide just far enough to show option `i` whole, from
  where it is headed."
  [app i]
  (let [{:keys [row-h rows]} (place app)
        target (get-in app [:list :target])
        top    (* i row-h)
        bottom (+ top row-h)]
    (head-for app (cond (< top target)                    top
                        (> bottom (+ target (* rows row-h))) (- bottom (* rows row-h))
                        :else                             target))))

(defn- activate [app i]
  (if (= i (get-in app [:list :active]))
    app
    (-> app (assoc-in [:list :active] i) (assoc :dirty? true))))

(defn- follow-pointer
  "While the pointer leads, put the list on the option under it, if any:
  as the list glides under a still pointer, that changes."
  [app]
  (if-let [i (when-let [[x y] (get-in app [:list :pointer])] (row-at app x y))]
    (activate app i)
    app))

(defn glide
  "The list moved on towards where it is headed, as of `now`: the
  distance left shrinks by e for every :list-glide-ms, and it arrives
  once within half a pixel."
  [app now]
  (if-let [{:keys [scroll target at]} (:list app)]
    (if (= scroll target)
      (assoc-in app [:list :at] now)
      (let [left (* (- scroll target) (Math/exp (- (/ (- now at) (double (:list-glide-ms app))))))]
        (-> app
            (update :list assoc :scroll (if (< (Math/abs left) 0.5) target (+ target left)) :at now)
            (assoc :dirty? true)
            follow-pointer)))
    app))

(defn gliding?
  "Whether the list is still on its way to where it is headed."
  [app]
  (when-let [{:keys [scroll target]} (:list app)] (not= scroll target)))

;; ---------------------------------------------------------------- opening

(defn open
  "Open the list of the dropdown with the focus, on the option it holds,
  scrolled straight to show that in the middle. A dropdown without
  options has no list."
  [app now]
  (let [{:keys [node]} (dropdown app)
        options (:options node)]
    (if (empty? options)
      app
      (let [i   (first (keep-indexed #(when (= %2 (ui-value app node)) %1) options))
            app (assoc app :list {:id (:id node) :active i :scroll 0.0 :target 0.0 :at now
                                  :typed "" :typed-at now}
                       :dirty? true)
            {:keys [row-h rows]} (place app)
            app (head-for app (* row-h (- (or i 0) (quot (dec rows) 2))))]
        (assoc-in app [:list :scroll] (get-in app [:list :target]))))))

(defn close [app]
  (if (:list app) (-> app (dissoc :list) (assoc :dirty? true)) app))

(defn- choose
  "Close the list, and have its dropdown hold option `i` (if any)."
  [app now i]
  (let [{:keys [node]} (dropdown app)
        app (close app)]
    (if-let [v (when i (nth (vec (:options node)) i nil))]
      (settings-window/set-value app now (:id node) v)
      app)))

;; ---------------------------------------------------------------- input

(defn- move
  "Put the list on option `i`, kept within the options, and show it. The
  keys lead now, not the pointer."
  [app i]
  (let [i (clamp i 0 (dec (count (:options (place app)))))]
    (-> app (assoc-in [:list :pointer] nil) (activate i) (reveal i))))

(defn- on-key
  "A key while the list is open; nil for tab, which the focus takes."
  [app now key]
  (let [{:keys [active]} (:list app)
        {:keys [rows options]} (place app)
        page (max 1 (dec rows))]
    (condp = key
      sdl/K-ESCAPE   (close app)
      sdl/K-RETURN   (choose app now active)
      sdl/K-KP-ENTER (choose app now active)
      sdl/K-TAB      nil
      sdl/K-UP       (move app (if active (dec active) (dec (count options))))
      sdl/K-DOWN     (move app (if active (inc active) 0))
      sdl/K-PAGEUP   (move app (- (or active 0) page))
      sdl/K-PAGEDOWN (move app (+ (or active 0) page))
      sdl/K-HOME     (move app 0)
      sdl/K-END      (move app (dec (count options)))
      app)))

(defn- typing?
  "Whether a name is being typed: there is one, typed recently enough."
  [app now]
  (let [{:keys [typed typed-at]} (:list app)]
    (and (seq typed) (< (- now typed-at) (:type-ahead-ms app)))))

(defn on-text
  "Typed `text` while the list is open: onto the name being typed, going
  to the first option that starts with it. A space that starts a name
  chooses the option the list is on instead."
  [app now text]
  (if (and (= " " text) (not (typing? app now)))
    (choose app now (get-in app [:list :active]))
    (let [typed  (str (when (typing? app now) (get-in app [:list :typed])) text)
          app    (update app :list assoc :typed typed :typed-at now)
          prefix (str/lower-case typed)
          i (first (keep-indexed #(when (str/starts-with? (str/lower-case %2) prefix) %1)
                                 (:options (place app))))]
      (if i (move app i) app))))

(defn on-event
  "An event while the list is open: it takes them all, but for those it
  leaves to the rest of the app, for which nil."
  [app now event]
  (let [{:keys [x y]} event]
    (case (:type event)
      :key   (on-key app now (:key event))
      :text  (on-text app now (:text event))
      :click (if-let [i (row-at app x y)]
               (choose app now i)
               (if (in-rect? (:rect (place app)) x y) app (close app)))
      :move  (let [over? (in-rect? (:rect (place app)) x y)]
               (-> app
                   (assoc-in [:list :pointer] (when over? [x y]))
                   follow-pointer
                   ;; the boxes the list covers aren't under the pointer
                   (hover (when-not over? x) y)))
      :wheel (head-for app (- (get-in app [:list :target])
                              (* (:dy event) (:wheel-lines app) (row-height app))))
      (:drag :release :composition) app
      nil)))
