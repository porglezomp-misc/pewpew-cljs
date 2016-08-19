(ns pewpew.physics
  (:require [pewpew.collision :as collision]
            [pewpew.bbox :as bbox]
            [pewpew.util :as util :refer [indexed floor]]))

(defn merge-matrices
  "Takes a sequence of matrices (a (vec (vec item))) and returns a
  matrix containing vectors of the respective elements of each matrix,
  dropping any nil elements or nil matrices in the process."
  [matrices]
  (when-not (empty? matrices)
    (let [filter-falsy (partial keep identity)
          non-nil-vec (comp vec filter-falsy vector)
          merge-row (partial (comp vec map) non-nil-vec)
          merge-matrix (partial (comp vec map) merge-row)]
      (->> matrices
           filter-falsy
           (apply merge-matrix)))))

(defn make-collision-grid
  [layers]
  (merge-matrices
   (for [layer layers]
     (when (get-in layer [:properties :solid])
       (for [[rowid row] (indexed (:data layer))]
         (for [[colid tile] (indexed row)]
           (when (pos? tile)
             (let [x0 colid y0 rowid
                   x1 (+ x0 1) y1 (+ y0 1)]
               {:tile tile
                :bbox [x0 y0 x1 y1]}))))))))

(defonce collision-grid (atom nil))


(defn get-colliders
  [box]
  (when-let [[x0 y0 x1 y1] (map floor (bbox/bbox box))]
    (mapcat identity
            (for [row (range y0 (+ y1 1))
                  col (range x0 (+ x1 1))]
              (get-in @collision-grid [row col])))))

(defn resolve-collision
  [dynamic statics]
  (loop [x 0
         y 0
         [static & statics] statics]
    (let [new-dynamic (bbox/offset-by dynamic x y)]
      (if-not static
        [x y]
        (if-let [[dx dy] (collision/liberate-motion new-dynamic (:bbox static))]
          (recur (+ x dx) (+ y dy) statics))))))

(defn adjust-delta
  [delta push]
  (cond
    (pos? push) (max 0 delta)
    (neg? push) (min 0 delta)
    :else delta))

(defn update-player
  [player dt]
  (let [next-dy (-> (:dy player)
                    (- (* 9.8 dt)))
        next-x (+ (:x player) (* (:dx player) dt))
        next-y (+ (:y player) (* next-dy dt))
        player-bbox (bbox/offset-by (:bbox player) next-x next-y)
        colliders (get-colliders player-bbox)
        [offset-x offset-y] (resolve-collision player-bbox colliders)
        next-x (+ offset-x next-x)
        next-y (+ offset-y next-y)
        grounded (> offset-y 0)
        next-dx (-> (:dx player)
                    (* (if grounded
                         (- 1 (* 5.0 dt))
                         (- 1 (* 0.01 dt))))
                    (adjust-delta offset-x))
        next-dy (adjust-delta next-dy offset-y)]
    (when (> next-y -3)
      (assoc player
             :x next-x :y next-y
             :dx next-dx :dy next-dy
             :grounded grounded))))
