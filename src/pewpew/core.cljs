(ns pewpew.core
  (:require [cljs.core.async :as async :refer [<! >!]]
            cljsjs.react-pixi
            [pewpew.bbox :as bbox]
            [pewpew.collision :as collision]
            [pewpew.interval :as interval]
            [pewpew.util :as util :refer [indexed dbg log]]
            [reagent.core :as r]
            [taoensso.sente :as sente]
            [tmx.core :as tmx])
  (:require-macros
   [cljs.core.async.macros :refer [go]]
   [reagent.ratom :refer [reaction]]
   #_[devcards.core :as dc :refer [defcard-rg]]))

(def stage (r/adapt-react-class js/ReactPIXI.Stage))
(def sprite (r/adapt-react-class js/ReactPIXI.Sprite))
(def container (r/adapt-react-class js/ReactPIXI.DisplayObjectContainer))

(set! js/PIXI.SCALE_MODES.DEFAULT js/PIXI.SCALE_MODES.NEAREST)

;; define your app data so that it doesn't get over-written on reload
(defonce game-world (r/atom {:tilemap nil}))

(defn make-world-coordinate-scale []
  (js/PIXI.Point. 16 -16))
(def world-coordinate-scale (r/track! make-world-coordinate-scale))

(defn make-object-coordinate-scale []
  (js/PIXI.Point. (/ 1 16) (/ -1 16)))
(def object-coordinate-scale (r/track! make-object-coordinate-scale))

(def tilemap-texture (.fromImage js/PIXI.Texture "/img/tileset0.png"))

(defn make-tile-texture
  [base x y w h]
  (let [x (* x w)
        y (* y h)
        frame (js/PIXI.Rectangle. x y w h)]
    (js/PIXI.Texture. tilemap-texture frame)))

(def tile-textures
  (reaction
   (when-let [tilesets (get-in @game-world [:tilemap :tilesets])]
     (apply merge
            (for [tileset tilesets
                  tileid (range (:tilecount tileset))]
              (let [id (+ (:firstgid tileset) tileid)
                    [w h] (:tilesize tileset)
                    [x y] ((juxt mod quot) tileid (:rows tileset))]
                {id (make-tile-texture tilemap-texture x y w h)}))))))

(def spawn-points
  (reaction
   (when-let [objects (get-in @game-world [:tilemap :objects])]
     (filter #(-> % :type (= "Spawn")) objects))))

(def player-texture (.fromImage js/PIXI.Texture "/img/player0.png"))

(def player-frames
  (vec (for [i (range 4)]
         (js/PIXI.Texture. player-texture (js/PIXI.Rectangle. (* 16 i) 0 16 16)))))

(defn merge-matrices
  "Takes a sequence of matrices (a (vec (vec item))) and returns a
  matrix containing vectors of the respective elements of each matrix,
  dropping any nil elements or nil matrices in the process."
  [matrices]
  (when-not (empty? matrices)
    (let [filter-falsy (partial filter identity)
          non-nil-vec (comp vec filter-falsy vector)
          merge-row (partial (comp vec map) non-nil-vec)
          merge-matrix (partial (comp vec map) merge-row)]
      (->> matrices
           filter-falsy
           (apply merge-matrix)))))

(def tile-width (r/cursor game-world [:tilemap :tile-width]))
(def tile-height (r/cursor game-world [:tilemap :tile-height]))

(defn make-collision-grid
  []
  (let [layers (get-in @game-world [:tilemap :layers])
        tile-width @tile-width
        tile-height @tile-height]
    (merge-matrices
     (for [layer layers]
       (when (get-in layer [:properties :solid] false)
         (for [[rowid row] (indexed (:data layer))]
           (for [[colid tile] (indexed row)]
             (when (pos? tile)
               (let [x0 (* colid tile-width)
                     y0 (* rowid tile-width)
                     x1 (+ x0 tile-width)
                     y1 (+ y0 tile-height)]
                 {:tile tile
                  :bbox [x0 y0 x1 y1]})))))))))

(def collision-grid (r/track! make-collision-grid))

(defn tile
  [{:keys [x y texture]}]
  (let [texture (get @tile-textures texture)]
    [sprite {:texture texture :x x :y y :scale @object-coordinate-scale}]))

(defn tile-row
  [i row]
  [container {:y i}
   (for [[i tile-id] (indexed row)]
     (when (pos? tile-id)
       ^{:key i} [tile {:x i :y 0 :texture tile-id}]))])

(defn layer
  [the-layer]
  [container {}
   (for [[i row] (indexed (:data the-layer))]
     ^{:key i} [tile-row i row])])

(defn tilemap
  [the-map]
  [container {}
   (for [the-layer (:layers the-map)]
     ^{:key (:name the-layer)} [layer the-layer])])

(defn player
  [{:keys [x y]}]
  [sprite {:x x :y y
           :texture (player-frames 0)
           :scale @object-coordinate-scale
           :anchor (js/PIXI.Point. 0.5 0)}])

(defn game-root
  [world]
  [stage {:width 400 :height 300
          :backgroundcolor (some-> (get-in @world [:tilemap :background-color])
                                   (clojure.string/replace #"#" "0x")
                                   js/Number)}
   [container {:x 8 :y (- 300 16) :scale @world-coordinate-scale}
    [tilemap (:tilemap @world)]
    (for [[i the-player] (indexed (:players @world))]
      ^{:key i} [player the-player])]])

(r/render-component [game-root game-world]
                    (. js/document (getElementById "app")))

(defn on-js-reload
  []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  #_(swap! app-state update-in [:__figwheel_counter] inc))

(defn load-map!
  [map-url]
  (go
    (let [tilemap (<! (tmx/url->tmx map-url))
          flipped-layers (->> (:layers tilemap)
                              (map #(update-in % [:data] reverse))
                              vec)
          tilemap (assoc tilemap :layers flipped-layers)]
      (swap! game-world assoc-in [:tilemap] tilemap))))

(load-map! "/map.tmx")
