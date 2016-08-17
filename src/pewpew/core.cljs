(ns pewpew.core
  (:require [cljs.core.async :as async :refer [<! >!]]
            [impi.core :as impi]
            [pewpew.bbox :as bbox]
            [pewpew.collision :as collision]
            [pewpew.interval :as interval]
            [pewpew.util :as util :refer [ceil dbg floor indexed log]]
            [taoensso.sente :as sente]
            [tmx.core :as tmx])
  (:require-macros
   [cljs.core.async.macros :refer [go]]))

(defonce game-world (atom {:tilemap nil :players [] :tile-textures {}}))

(defn make-spawn-points
  [objects]
  (filter #(-> % :type (= "Spawn")) objects))

(defonce spawn-points (atom []))
(add-watch game-world ::spawn-points
           (fn [_ _ old new]
             (let [old (get-in old [:tilemap :objects])
                   new (get-in new [:tilemap :objects])]
               (if (not= old new)
                 (reset! spawn-points (make-spawn-points new))))))

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
(add-watch game-world ::collision-grid
           (fn [_ _ old new]
             (let [old (get-in old [:tilemap :layers])
                   new (get-in new [:tilemap :layers])]
               (if (not= old new)
                 (reset! collision-grid (make-collision-grid new))))))

(defn make-tile-textures
  [tilesets]
  (reduce merge
          (for [tileset tilesets
                id (range (:tilecount tileset))]
            (let [[w h] (:tilesize tileset)
                  cols (:columns tileset)
                  start-id (:firstgid tileset)
                  row (quot id cols)
                  col (mod id cols)]
              {(+ id start-id)
               {:pixi.texture/source (str "/" (:image-source tileset))
                :pixi.texture/frame [(* col w) (* row h) w h]}}))))

(add-watch game-world ::tile-textures
           (fn [_ _ old new]
             (let [old (get-in old [:tilemap :tilesets])
                   new (get-in new [:tilemap :tilesets])]
               (if (not= old new)
                 (swap! game-world assoc :tile-textures (make-tile-textures new))))))

(defn tile-texture
  [tile-id]
  (get-in @game-world [:tile-textures tile-id]
       {:pixi.texture/source "/img/tileset0.png"
        :pixi.texture/frame [0 0 16 16]}))

(defn tile
  [x y tile-id]
  {:impi/key (keyword (str "tile" x "x" y))
   :pixi.object/type :pixi.object.type/sprite
   :pixi.object/scale (map / [16 -16])
   :pixi.object/position [x y]
   :pixi.sprite/texture (tile-texture tile-id )})

(defn layer
  [{:keys [name data]}]
  {:impi/key name
   :pixi.object/type :pixi.object.type/container
   :pixi.container/children
   (keep identity
         (for [[y row]     (indexed data)
               [x tile-id] (indexed row)]
           (when (pos? tile-id)
             (tile x y tile-id))))})

(defn player
  [{:keys [x y bbox id]}]
  {:impi/key (keyword (str "player" id))
   :pixi.object/type :pixi.object.type/sprite
   :pixi.object/position [x y]
   :pixi.object/rotation 0
   :pixi.object/scale (map / [16 -16])
   :pixi.sprite/anchor [0.5 0]
   :pixi.sprite/texture {:pixi.texture/source "/img/player0.png"
                         :pixi.texture/frame [0 0 16 16]}})

(defn root
  [world]
  {:pixi/renderer
   {:pixi.renderer/size             [400 300]
    :pixi.renderer/background-color (-> (get-in world [:tilemap :background-color])
                                        (clojure.string/replace #"#" "0x")
                                        js/Number)}
   :pixi/stage
   {:impi/key         :stage
    :pixi.object/type :pixi.object.type/container
    :pixi.object/scale [16 -16]
    :pixi.object/position [8 (- 300 24)]
    :pixi.container/children
    (array-map
     :layers
     {:impi/key :layers
      :pixi.object/type :pixi.object.type/container
      :pixi.container/children
      (for [the-layer (get-in world [:tilemap :layers])]
        (layer the-layer))}
     :players
     {:impi/key :players
      :pixi.object/type :pixi.object.type/container
      :pixi.container/children
      (for [[i the-player] (indexed (:players world))]
        (player (assoc the-player :id i)))})}})

(let [element (.getElementById js/document "app")]
  (impi/mount :example @game-world element)
  (add-watch game-world ::mount
             (fn [_ _ old new]
               (when (not= old new)
                 (impi/mount :example (root new) element)))))

(defn on-js-reload [] nil)

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
        next-dx (-> (:dx player)
                    (* (if (not= offset-y 0)
                         (- 1 (* 0.1 dt))
                         (- 1 (* 0.01 dt))))
                    (adjust-delta offset-x))
        next-dy (adjust-delta next-dy offset-y)]
    (when (> next-y -3)
      (assoc player :x next-x :y next-y :dx next-dx :dy next-dy))))

(defn do-update
  [world dt]
  (let [players (->> (:players world)
                     (map #(update-player % dt))
                     (filter identity))]
    (assoc-in world [:players] (vec players))))

(def target-fps 60)
(def timestep (/ target-fps))

(defonce update! (atom nil))
(defn -update!
  [extra-time last-timestamp timestamp]
  (let [dt (/ (- timestamp last-timestamp) 1000)
        extra-time (loop [extra-time (+ extra-time dt)]
                     (cond
                       (< extra-time timestep) extra-time
                       (> extra-time (* timestep 16)) (do (log "FALLING BEHIND!" dt) 0)
                       :else (do (swap! game-world #(do-update % timestep))
                                 (recur (- extra-time timestep)))))]
    (js/requestAnimationFrame (partial @update! extra-time timestamp))))

(reset! update! -update!)
(defonce update-loop (js/requestAnimationFrame (partial @update! 0 0)))

(defn load-map!
  [map-url]
  (go
    (let [tilemap (<! (tmx/url->tmx map-url))
          flipped-layers (->> (:layers tilemap)
                              (map #(update-in % [:data] reverse))
                              vec)
          tilemap (assoc tilemap :layers flipped-layers)]
      (swap! game-world assoc :tilemap tilemap))))

(load-map! "/map.tmx")
