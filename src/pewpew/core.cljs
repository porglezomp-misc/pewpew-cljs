(ns pewpew.core
  (:require [cljs.core.async :as async :refer [<! >!]]
            cljsjs.react-pixi
            [reagent.core :as r]
            [taoensso.sente :as sente]
            [tmx.core :as tmx])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [reagent.ratom :refer [reaction]]))

(enable-console-print!)

(def stage (r/adapt-react-class js/ReactPIXI.Stage))
(def sprite (r/adapt-react-class js/ReactPIXI.Sprite))
(def container (r/adapt-react-class js/ReactPIXI.DisplayObjectContainer))

(defn indexed [s] (map-indexed vector s))

;; define your app data so that it doesn't get over-written on reload
(defonce game-world (r/atom {:tilemap nil}))
(def tilemap-texture (.fromImage js/PIXI.Texture "/img/tileset0.png"))

(defn make-tile-texture
  [base x y w h]
  (let [x (* x w)
        y (* y h)
        frame (js/PIXI.Rectangle. x y w h)]
    (js/PIXI.Texture. tilemap-texture frame)))

(defn make-tile-textures
  []
  (let [tilesets @(r/cursor game-world [:tilemap :tilesets])]
    (apply merge
      (for [tileset tilesets
            tileid (range (:tilecount tileset))]
        (let [id (+ (:firstgid tileset) tileid)
              [w h] (:tilesize tileset)
              [x y] ((juxt mod quot) tileid (:rows tileset))]
          {id (make-tile-texture tilemap-texture x y w h)})))))

(def tile-textures
  (r/track! make-tile-textures))

(defn tile
  [{:keys [x y texture]}]
  (let [texture (@tile-textures texture)]
    [sprite {:texture texture :x x :y y}]))

(defn tile-row
  [i row]
  [container {:x 0 :y (* i 16)}
   (for [[i tile-id] (indexed row)]
     (when (pos? tile-id)
       ^{:key i} [tile {:x (* i 16) :y 0 :texture tile-id}]))])

(defn layer
  [the-layer]
  [container {:x 0 :y 0}
   (for [[i row] (indexed (:data the-layer))]
     ^{:key i} [tile-row i row])])

(defn tilemap
  [the-map]
  [container {:x 0 :y 0}
   (for [the-layer (:layers the-map)]
     ^{:key (:name the-layer)} [layer the-layer])])

(defn game-root
  [world]
  [stage {:width 500 :height :300}
   [tilemap (:tilemap @world)]])

(r/render-component [game-root game-world]
                    (. js/document (getElementById "app")))

(defn on-js-reload
  []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  #_(swap! app-state update-in [:__figwheel_counter] inc))

(go
  (let [tilemap (<! (tmx/url->tmx "/map.tmx"))]
    (swap! game-world assoc-in [:tilemap] tilemap)))
