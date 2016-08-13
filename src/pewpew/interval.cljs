(ns pewpew.interval
  (:require-macros
   [cljs.test :refer [is]]
   [devcards.core :as dc :refer [deftest]]))

(defn interval
  "Returns an interval where the first number is smaller, or nil. If passed a
  single number, returns an empty interval representing that point, like [3 3]."
  ([xs]
   (cond
     (nil? xs) nil
     (coll? xs) (when-let [[x0 x1] xs]
                  (interval x0 x1))
     :else [xs xs]))
  ([x0 x1]
   [(min x0 x1) (max x0 x1)]))

(deftest interval-test
  "The `interval` function ensures that the two elements are correctly ordered."
  (is (= (interval 0 10) [0 10]))
  (is (= (interval nil) nil))
  "It can be called either on a vec or with two arguments."
  (is (= (interval 10 0) (interval [10 0])))
  "When `interval` is called on a single number it returns an empty interval
  representing that point."
  (is (= (interval 3) (interval 3 3))))

(defn length
  "Returns the distance between the first and last endpoints of the interval.
  For empty intervals representing points, returns 0, for nil intervals, returns
  nil."
  [x]
  (if-let [[x0 x1] (interval x)]
    (- x1 x0)))

(deftest length-test
  "The `length` function returns the distance between the first and last
  endpoints of an interval."
  (is (= (length (interval 3 10)) 7))
  "When `length` is called on an empty interval, it returns 0, and when called
  on `nil` it returns `nil`."
  (is (= (length (interval 2)) 0))
  (is (= (length nil) nil))
  "It can correctly handle any input convertible to an interval."
  (is (= (length 3) 0))
  (is (= (length [5 1]) 4)))

(defn union
  "Returns the smallest interval that contains both the input intervals. If one
  of the input intervals is nil, returns the other one."
  [a b]
  (or (when-let [[a0 a1] (interval a)]
        (when-let [[b0 b1] (interval b)]
          [(min a0 b0) (max a1 b1)]))
      a b))

(deftest union-test
  "When one of the intervals passed to `union` is `nil` it will return the other
  argument."
  (is (= (union nil nil) nil))
  (is (= (union [0 10] nil) [0 10]))
  (is (= (union nil [0 10]) [0 10]))
  "When one of the intervals is entirely contained inside the other, the
  containing interval is returned."
  (is (= (union [0 100] [10 20]) [0 100]))
  "The `union` function will return the smallest interval that contains both of
  the input intervals."
  (is (= (union [0 10] [20 30]) [0 30]))
  "`union` can correctly accept malformed intervals as input."
  (is (= (union [30 20] [0 10]) [0 30])))

(defn intersection
  [a b]
  (let [[a b] (sort-by first [(interval a) (interval b)])]
    (when-let [[a0 a1] a]
      (when-let [[b0 b1] b]
        (when (>= a1 b0)
          [b0 (min a1 b1)])))))

(deftest intersection-test
  "When one of the intervals passed to `intersection` is `nil` it will return
  `nil`."
  (is (= (intersection nil nil) nil))
  (is (= (intersection nil [0 10]) nil))
  (is (= (intersection [0 10] nil) nil))
  "If the two intervals do not overlap, `intersection` will return `nil`."
  (is (= (intersection [0 10] [20 30]) nil))
  "The `intersection` of an interval with itself is itself."
  (is (= (intersection [0 10] [0 10]) [0 10]))
  "If the two intervals overlap at the end, they will produce a degenerate
    interval that represents a single point."
  (is (= (intersection [0 10] [10 20]) [10 10]))
  "`intersection` can correctly accept malformed intervals as input."
  (is (= (intersection [30 10] [20 0]) [10 20])))
