(ns pewpew.core
  (:require [cljs.core.async :as async :refer [<! >!]]
            [impi.core :as impi]
            [pewpew.bbox :as bbox]
            [pewpew.collision :as collision]
            [pewpew.interval :as interval]
            [pewpew.util :as util :refer [ceil dbg floor indexed log]]
            [taoensso.sente :as sente]
            [tmx.core :as tmx]
            [pewpew.physics :as physics])
  (:require-macros
   [cljs.core.async.macros :refer [go]]))

(defonce game-world (atom {:tilemap nil :players [] :tile-textures {}}))
(defonce pressed-keys (atom #{}))

(defn make-spawn-points
  [objects]
  (filter #(-> % :type (= "Spawn")) objects))

(defonce spawn-points (atom []))
(add-watch game-world ::spawn-points
           (fn [_ _ old new]
             (let [old (get-in old [:tilemap :objects])
                   new (get-in new [:tilemap :objects])]
               (if (not= old new)
                 (reset! spawn-points
                         (make-spawn-points new))))))

(add-watch game-world ::collision-grid
           (fn [_ _ old new]
             (let [old (get-in old [:tilemap :layers])
                   new (get-in new [:tilemap :layers])]
               (if (not= old new)
                 (reset! physics/collision-grid
                         (physics/make-collision-grid new))))))

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
   :pixi.sprite/texture (tile-texture tile-id)})

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
  [{:keys [x y bbox id flip]}]
  {:impi/key (keyword (str "player" id))
   :pixi.object/type :pixi.object.type/sprite
   :pixi.object/position [x y]
   :pixi.object/rotation 0
   :pixi.object/scale (map / [(* (or flip 1) 16) -16])
   :pixi.sprite/anchor [0.5 0]
   :pixi.sprite/texture {:pixi.texture/source "/img/player0.png"
                         :pixi.texture/frame [0 0 16 16]}})

(defn root
  [world]
  {:pixi/renderer
   {:pixi.renderer/size             [400 300]
    :pixi.renderer/background-color (some-> (get-in world [:tilemap :background-color])
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
  (impi/mount :example (root @game-world) element)
  (add-watch game-world ::mount
             (fn [_ _ old new]
               (when (not= old new)
                 (impi/mount :example (root new) element)))))

(defn on-js-reload [] nil)

(defn update-player
  [player]
  (let [keys @pressed-keys]
    (cond-> player
      (and (keys :key/left) (not (keys :key/right)))
      (assoc :flip -1 :dx -3)

      (and (keys :key/right) (not (keys :key/left)))
      (assoc :flip 1 :dx 3)

      (and (keys :key/up) (:grounded player))
      (assoc :dy 6))))

(defn do-update
  [world dt]
  (let [players (->> (:players world)
                     (map update-player)
                     (map #(physics/update-player % dt))
                     (keep identity))]
    (assoc-in world [:players] (vec players))))

(def target-fps 60)
(def timestep (/ target-fps))

(defonce update! (atom nil))
(defn -update!
  [extra-time last-timestamp timestamp]
  (let [dt (/ (- timestamp last-timestamp) 1000)
        [extra-time world] (loop [extra-time (+ extra-time dt)
                                  world @game-world]
                             (cond
                               (< extra-time timestep)
                               [extra-time world]

                               (> extra-time (* timestep 16))
                               (do (log "FALLING BEHIND!" dt)
                                   [0 world])

                               :else
                               (recur (- extra-time timestep)
                                      (do-update world timestep))))]
    (reset! game-world world)
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

(defn str->key
  [key-name]
  (cond
    (= key-name " ")
    :key/space

    (clojure.string/starts-with? key-name "Arrow")
    (-> key-name
        (clojure.string/replace #"Arrow" "")
        clojure.string/lower-case
        (->> (keyword "key")))

    :else
    (-> key-name
        (clojure.string/split #"(?=[A-Z])")
        (->>
         (map clojure.string/lower-case)
         (clojure.string/join "-")
         (keyword "key")))))

(defn key-down
  [event]
  (let [key (str->key (.-key event))]
    (swap! pressed-keys #(conj % key))))

(defn key-up
  [event]
  (let [key (str->key (.-key event))]
    (swap! pressed-keys #(disj % key))))

(defonce event-listeners
  (do
    (js/addEventListener "keydown" key-down)
    (js/addEventListener "keyup" key-up)
    :done))

(defn add-player!
  [new-player]
  (swap! game-world update-in [:players] #(conj % new-player)))
