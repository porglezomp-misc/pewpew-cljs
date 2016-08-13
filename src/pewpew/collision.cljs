(ns pewpew.collision
  (:require [pewpew.bbox :as bbox]
            [pewpew.interval :as interval]
            [pewpew.util :as util]
            [reagent.core :as r])
  (:require-macros
   [cljs.test :refer [is deftest]]
   #_[devcards.core :as dc :refer [deftest defcard defcard-rg]]))

(defn overlap?
  "Returns the overlap of two bounding boxes, or nil if they don't overlap."
  [a b]
  (bbox/intersection a b))

(defn liberate-motion
  "Returns the required motion to move bounding box a outside of bounding box b."
  [a b]
  (when-let [overlap (overlap? a b)]
    (let [x-overlap (interval/length (bbox/x-interval overlap))
          y-overlap (interval/length (bbox/y-interval overlap))
          [ax ay] (bbox/center a)
          [bx by] (bbox/center b)]
      (if (< x-overlap y-overlap)
        (if (< ax bx) [(- x-overlap) 0] [x-overlap 0])
        (if (< ay by) [0 (- y-overlap)] [0 y-overlap])))))

(deftest liberate-motion-test
  "Two non-overlapping bounding boxes have a required motion of `nil`."
  (is (= (liberate-motion [0 0 10 10] [20 20 30 30]) nil))
  "Two overlapping bounding boxes have a non `nil` required motion."
  (is (not= nil (liberate-motion [0 0 10 10] [0 5 10 15])))
  "The required motion is for the first bounding box, with the assumption that
  the second bounding box is static. `liberate-motion` will provide the
  displacement along the axis with the least overlap."
  (is (= (liberate-motion [0 0 10 10] [0 5 10 15]) [0 -5]))
  (is (= (liberate-motion [0 0 10 10] [5 0 15 10]) [-5 0]))
  "The displacement will always be in the direction that minimizes the motion."
  (is (= (liberate-motion [0 0 10 10] [0 1 10 11]) [0 -9]))
  (is (= (liberate-motion [0 1 10 11] [0 0 10 10]) [0 9])))

#_(defcard-rg box-resolution
  (let [fixed [0 30 30 60]
        a [5 10 35 40]
        b [10 35 40 65]]
    [util/boxes-component {:width 60 :height 65}
     {:box fixed}
     {:box a :style {:border "dashed red 1px"}}
     {:box (bbox/offset-by a (liberate-motion a fixed))}
     {:box b :style {:border "dashed red 1px"}}
     {:box (bbox/offset-by b (liberate-motion b fixed))}]))
