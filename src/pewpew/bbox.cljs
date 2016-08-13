(ns pewpew.bbox
  (:require [pewpew.interval :as interval]
            [pewpew.util :as util])
  (:require-macros
   [cljs.test :refer [is]]
   [devcards.core :as dc :refer [deftest defcard-rg]]))

(defn bbox
  "Returns a normalized bounding box."
  ([xs]
   (case (count xs)
     2 (let [[x y] xs]
         [x y x y])
     4 (let [[x0 y0 x1 y1] xs]
         [(min x0 x1) (min y0 y1) (max x0 x1) (max y0 y1)])
     nil))
  ([x y & xs]
   (bbox (into [x y] xs))))

(deftest bbox-test
  "The `bbox` function normalizes an input bounding box. It will guarantee that
  the first coordinates are smaller than the second coordinates."
  (is (= (bbox [10 10 0 0]) [0 0 10 10]))
  "It passes empty boxes (`nil`) and anything not recognizable as a bounding box
  as `nil`."
  (is (= (bbox nil) nil))
  (is (= (bbox [1]) nil))
  "Passing multiple arguments is equivalent to passing them in a vector."
  (is (= (bbox 10 0 0 10) (bbox [10 0 0 10])))
  "Passing a single point to `bbox` produces an empty box at that point."
  (is (= (bbox [2 3]) [2 3 2 3])))

(defn x-interval
  "Returns an interval representing the horizontal extent of the bounding box."
  [box]
  (when-let [[x0 _ x1 _] (bbox box)]
    (interval/interval x0 x1)))

(defn y-interval
  "Returns an interval representing the vertical extent of the bounding box."
  [box]
  (when-let [[_ y0 _ y1] (bbox box)]
    (interval/interval y0 y1)))

(deftest dimension-interval-test
  "The horizontal and vertical intervals of a bounding box can be extracted with `x-interval` and `y-interval`."
  (is (= (x-interval [0 30 10 40]) [0 10]))
  (is (= (y-interval [0 30 10 40]) [30 40]))
  "The intervals of a `nil` box are `nil`."
  (is (= (x-interval nil) nil))
  (is (= (y-interval nil) nil))
  "Degenerate boxes produce point intervals."
  (is (= (x-interval [0 10]) (interval/interval 0)))
  (is (= (y-interval [0 10]) (interval/interval 10))))

(defn center
  "Returns the point representing the center of the bounding box."
  [box]
  (when-let [[x0 y0 x1 y1] (bbox box)]
    [(/ (+ x0 x1) 2)
     (/ (+ y0 y1) 2)]))

(deftest center-test
  (is (= (center [0 0 10 10]) [5 5]))
  (is (= (center nil) nil))
  (is (= (center [10 10 0 0]) [5 5]))
  (is (= (center [4 2]) [4 2])))

(defn offset-by
  "Returns a new bounding box offset from the original."
  ([box offset]
   (let [[x y] offset]
     (offset-by box x y)))
  ([box x y]
   (when-let [[x0 y0 x1 y1] (bbox box)]
     [(+ x0 x) (+ y0 y) (+ x1 x) (+ y1 y)])))

(deftest offset-test
  (is (= (offset-by nil 10 10) nil))
  (is (= (offset-by [0 0 10 10] 5 0) [5 0 15 10]))
  (is (= (offset-by [0 0 10 10] 0 5) [0 5 10 15]))
  (is (= (offset-by [0 0] [4 2]) (bbox 4 2))))

(defn union
  "Returns the smallest bounding box that contains both bounding boxes."
  [a b]
  (or (when-let [[ax0 ay0 ax1 ay1] (bbox a)]
        (when-let [[bx0 by0 bx1 by1] (bbox b)]
          [(min ax0 bx0)
           (min ay0 by0)
           (max ax1 bx1)
           (max ay1 by1)]))
      a b))

(deftest union-test
  "The `union` of two non-rectangles gives you nothing."
  (is (= (union nil nil) nil))
  "The `union` of a rectangle and anything empty gives you the non-empty one."
  (is (= (union [0 0 10 10] nil) [0 0 10 10]))
  (is (= (union nil [10 25 35 90]) [10 25 35 90]))
  "The `union` of two bounding boxes is the smallest rectangle that contains
  them both."
  (is (= (union [0 0 10 10] [20 20 30 30]) [0 0 30 30]))
  (is (= (union [20 20 30 30] [0 0 10 10]) [0 0 30 30]))
  (is (= (union [0 0 10 10] [100 0 110 10]) [0 0 110 10]))
  "The `union` of a point and a bounding box contains the point and the bounding
  box."
  (is (= (union [0 0 10 10] [20 20]) [0 0 20 20]))
  "The `union` function will correctly handle malformed bounding boxes."
  (is (= (union [10 10 0 0] [20 20]) [0 0 20 20])))

(defcard-rg union-ex
  (let [a [0 0 30 30]
        b [20 20 40 40]
        c [80 0 120 30]
        d [85 5 110 15]
        e [150 0 180 20]
        f [160 25 190 40]]
    [util/boxes-component {:width 200 :height 50}
     {:box a}
     {:box b}
     {:box (union a b) :style {:border-color "#a44"}}
     {:box c}
     {:box d}
     {:box (union c d) :style {:border-color "#a44"}}
     {:box e}
     {:box f}
     {:box (union e f) :style {:border-color "#a44"}}]))

(defn intersection
  "Returns the bounding box that is entirely contained in both of the input
  bounding boxes"
  [a b]
  (when-let [[ax0 ay0 ax1 ay1] (bbox a)]
    (when-let [[bx0 by0 bx1 by1] (bbox b)]
      (when-let [[x0 x1] (interval/intersection [ax0 ax1] [bx0 bx1])]
        (when-let [[y0 y1] (interval/intersection [ay0 ay1] [by0 by1])]
          [x0 y0 x1 y1])))))

(deftest intersection-test
  "The `intersection` of two bounding boxes is the bounding box that is
  completely contained within both of them, or `nil` if they do not overlap at
  all. The `intersection` of a bounding box with itself is itself."
  (is (= (intersection [0 0 10 10] [0 0 10 10]) [0 0 10 10]))
  "The `intersection` of a bounding box with a non bounding box (like `nil`) is
  `nil`."
  (is (= (intersection [0 0 10 10] nil) nil))
  "When two bounding boxes don't overlap, the `intersection` is `nil`."
  (is (= (intersection [0 0 10 10] [20 20 30 30]) nil))
  "The `intersection` of a bounding box with something one that is contained
  entirely inside it is the contained bounding box."
  (is (= (intersection [0 0 30 30] [10 10 20 20]) [10 10 20 20]))
  (is (= (intersection [0 0 30 30] [10 10]) (bbox 10 10)))
  "If two bounding boxes overlap on only a single edge or vertex, they will
  produce the appropriate degenerate box."
  (is (= (intersection [0 0 10 10] [10 0 20 10]) [10 0 10 10]))
  (is (= (intersection [0 0 10 10] [10 10 20 20]) (bbox 10 10))))

(defcard-rg intersection-ex
  (let [a [0 0 30 30]
        b [20 20 40 40]
        c [80 0 120 30]
        d [85 5 110 15]
        e [150 0 180 20]
        f [160 25 190 40]]
    [util/boxes-component {:width 200 :height 50}
     {:box a}
     {:box b}
     {:box (intersection a b) :style {:background-color "#a44"}}
     {:box c}
     {:box d}
     {:box (intersection c d) :style {:background-color "#a44"}}
     {:box e}
     {:box f}
     {:box (intersection e f) :style {:background-color "#a44"}}]))
