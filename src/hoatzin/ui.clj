(ns hoatzin.ui
  "Boxes: graphical elements drawn in the text or above it.

  A node is plain data, {:kind k :style {...}} plus what its kind needs:
    {:kind :box :children [node ...]}        a container: lays out the
                                             children, nests to any depth
    {:kind :label :text s}                   text that can't be edited
    {:kind :field :id id :value s}           editable text; :value is its
                                             starting text, the app keeps
                                             what it holds now by :id
    {:kind :button :id id :text s}
    {:kind :checkbox :id id :value bool}
  Interactive nodes (fields, buttons, checkboxes) need an :id: the app keeps
  their values in a map by it, so a node stays a description that can be
  rebuilt freely without losing what was typed or ticked.

  Every kind takes the same :style, in points, CSS's border-box model:
    :width :height       outer size; otherwise the content's, plus insets
    :padding             n, [vertical horizontal] or [top right bottom left]
    :border              width; :border-color, :background [r g b]
    :color               text colour
    :position            :absolute takes the node out of its parent's flow
                         and places it at its :left :top :right :bottom
                         (any of them) inside the parent's border; with
                         both :left and :right (or :top and :bottom) and no
                         size, it stretches between them
  and a :box lays its in-flow children out as a flexbox does:
    :direction           :column (the default) or :row
    :gap                 between children
    :justify             along the direction: :start :center :end
                         :space-between
    :align               across it: :stretch (the default) :start :center :end
  while a child may set
    :grow                its share of the space left along the direction
    :align-self          to override its parent's :align
  Text is one line; it doesn't wrap. Children may overflow their parent.

  Placing is pure: given a context {:scale density, :text-size (fn [s] ->
  [w h] in pixels)}, `place` turns a node and a rect into a flat vector of
  {:node :rect :content} in paint order, every rect [x y w h] in pixels.
  hoatzin.app draws that vector and hit-tests it with `hit`.")

(def kind-styles
  "The style each kind starts from; a node's own :style overrides it."
  {:box      {}
   :label    {}
   :field    {:padding [3 5] :border 1}
   :button   {:padding [3 10] :border 1}
   :checkbox {:padding 2 :border 1}})

(def field-width "A field's content width, in points, when it sets no width." 160)
(def check-size "A checkbox's content size, in points." 10)

(defn style [node] (merge (kind-styles (:kind node)) (:style node)))

(defn interactive? [node] (contains? #{:field :button :checkbox} (:kind node)))

(defn- px ^long [ctx v] (long (Math/round (* (double v) (:scale ctx)))))

(defn- edges
  "Padding as [top right bottom left]."
  [v]
  (cond (nil? v)            [0 0 0 0]
        (number? v)         [v v v v]
        (= 2 (count v))     (let [[a b] v] [a b a b])
        :else               v))

(defn- insets
  "Border plus padding at each edge, [top right bottom left] in pixels."
  [ctx st]
  (let [b (px ctx (:border st 0))]
    (mapv #(+ b (px ctx %)) (edges (:padding st)))))

(defn- absolute? [st] (= :absolute (:position st)))

(defn- flow-children [node] (filterv #(not (absolute? (style %))) (:children node)))
(defn- absolute-children [node] (filterv #(absolute? (style %)) (:children node)))

;; ---------------------------------------------------------------- measuring

(declare measure)

(defn- content-size
  "The [w h] pixels a node's content wants, inside its insets."
  [ctx node st]
  (let [text-size (:text-size ctx)]
    (case (:kind node)
      (:label :button) (text-size (str (:text node)))
      :field    [(px ctx field-width) (second (text-size ""))]
      :checkbox (let [s (px ctx check-size)] [s s])
      :box      (let [row?  (= :row (:direction st))
                      sizes (mapv #(measure ctx %) (flow-children node))
                      gaps  (* (px ctx (:gap st 0)) (max 0 (dec (count sizes))))
                      along (reduce + gaps (map (if row? first second) sizes))
                      across (reduce max 0 (map (if row? second first) sizes))]
                  (if row? [along across] [across along])))))

(defn measure
  "The outer [w h] `node` wants, in pixels."
  [ctx node]
  (let [st (style node)
        [t r b l] (insets ctx st)
        [cw ch] (content-size ctx node st)]
    [(if-let [w (:width st)] (px ctx w) (+ l cw r))
     (if-let [h (:height st)] (px ctx h) (+ t ch b))]))

;; ---------------------------------------------------------------- placing

(defn- grown
  "`sizes` with `free` pixels shared out by `grows`, rounding as it goes so
  the shares add up to exactly `free`."
  [sizes grows total free]
  (loop [i 0, g 0.0, given 0, out []]
    (if (= i (count sizes))
      out
      (let [g (+ g (grows i))
            upto (long (Math/round (/ (* free g) total)))]
        (recur (inc i) g upto (conj out (+ (sizes i) (- upto given))))))))

(defn- flow-rects
  "[child rect] for each of the in-flow `children` of a box styled `st`,
  laid out in its content rect."
  [ctx st [cx cy cw ch] children]
  (let [row?    (= :row (:direction st))
        n       (count children)
        sizes   (mapv #(measure ctx %) children)
        styles  (mapv style children)
        along   (fn [[w h]] (if row? w h))
        across  (fn [[w h]] (if row? h w))
        avail   (if row? cw ch)
        breadth (if row? ch cw)
        gap     (px ctx (:gap st 0))
        free    (- avail (* gap (max 0 (dec n))) (reduce + 0 (map along sizes)))
        grows   (mapv #(double (:grow % 0)) styles)
        total   (reduce + 0.0 grows)
        grow?   (and (pos? free) (pos? total))
        mains   (if grow? (grown (mapv along sizes) grows total free) (mapv along sizes))
        left    (if grow? 0 free)
        justify (:justify st :start)
        ;; how far along child i starts, besides the children before it
        lead    (fn [i] (case justify
                          :end           left
                          :center        (quot left 2)
                          :space-between (if (and (> n 1) (pos? left)) (quot (* i left) (dec n)) 0)
                          0))]
    (loop [i 0, before 0, out []]
      (if (= i n)
        out
        (let [cst   (styles i)
              main  (mains i)
              align (or (:align-self cst) (:align st) :stretch)
              sized? (some? (if row? (:height cst) (:width cst)))
              cross (if (and (= align :stretch) (not sized?)) breadth (across (sizes i)))
              off   (case align :center (quot (- breadth cross) 2) :end (- breadth cross) 0)
              at    (+ before (* i gap) (lead i))]
          (recur (inc i) (+ before main)
                 (conj out [(children i) (if row?
                                           [(+ cx at) (+ cy off) main cross]
                                           [(+ cx off) (+ cy at) cross main])])))))))

(defn- axis
  "[start size] along one axis of an absolute child, from its offsets `a`
  (left/top) and `b` (right/bottom) in points, inside [origin, origin+avail)."
  [ctx a b sized? measured origin avail]
  (let [a (when a (px ctx a))
        b (when b (px ctx b))
        size (if (and a b (not sized?)) (max 0 (- avail a b)) measured)]
    [(cond a (+ origin a)
           b (- (+ origin avail) b size)
           :else origin)
     size]))

(defn- absolute-rect
  "Where an absolute `child` goes inside its parent's border, [x y w h]."
  [ctx [x y w h] child]
  (let [st (style child)
        [mw mh] (measure ctx child)
        [cx cw] (axis ctx (:left st) (:right st) (some? (:width st)) mw x w)
        [cy ch] (axis ctx (:top st) (:bottom st) (some? (:height st)) mh y h)]
    [cx cy cw ch]))

(defn- place* [ctx node [x y w h :as rect] acc]
  (let [st (style node)
        [t r b l] (insets ctx st)
        bw (px ctx (:border st 0))
        content [(+ x l) (+ y t) (max 0 (- w l r)) (max 0 (- h t b))]
        acc (conj! acc {:node node :rect rect :content content})]
    (if (= :box (:kind node))
      (let [acc (reduce (fn [acc [child r]] (place* ctx child r acc))
                        acc (flow-rects ctx st content (flow-children node)))
            inside [(+ x bw) (+ y bw) (max 0 (- w bw bw)) (max 0 (- h bw bw))]]
        (reduce (fn [acc child] (place* ctx child (absolute-rect ctx inside child) acc))
                acc (absolute-children node)))
      acc)))

(defn place
  "`node` and everything in it, placed in `rect` [x y w h]: a vector of
  {:node :rect :content} in paint order, each node before its children and
  a box's absolute children after its others. :content is the rect inside
  the node's border and padding."
  [ctx node rect]
  (persistent! (place* ctx node rect (transient []))))

(defn offset
  "Placed entries moved by (dx, dy)."
  [placed dx dy]
  (let [move (fn [[x y w h]] [(+ x dx) (+ y dy) w h])]
    (mapv #(-> % (update :rect move) (update :content move)) placed)))

(defn hit
  "The topmost of the placed entries whose rect holds point (x, y), or nil."
  [placed x y]
  (some (fn [{[rx ry rw rh] :rect :as e}]
          (when (and (<= rx x) (< x (+ rx rw)) (<= ry y) (< y (+ ry rh))) e))
        (rseq placed)))
