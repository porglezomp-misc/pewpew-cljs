(ns pewpew.core
  (:require [cljs.core.async :as async]
            cljsjs.react-pixi
            [reagent.core :as r]
            [taoensso.sente :as sente]))

(enable-console-print!)

(def stage (r/adapt-react-class js/ReactPIXI.Stage))
(def sprite (r/adapt-react-class js/ReactPIXI.Sprite))
(def container (r/adapt-react-class js/ReactPIXI.DisplayObjectContainer))

(def tilemap-image "/img/tiles.png")
(def tilemap-texture (.fromImage js/PIXI.Texture tilemap-image))
(defn make-tile-texture
  [x y]
  (let [x (* x 64)
        y (* y 64)
        frame (js/PIXI.Rectangle x y 64 64)]
    (js/PIXI.Texture. tilemap-texture frame)))

(def dumb-texture (make-tile-texture 0 0))

(def tile-textures
  {:dumb dumb-texture})

(defn tile
  [{:keys [x y texture]}]
  (let [x (* x 64)
        y (* y 64)
        texture (get tile-textures texture dumb-texture)]
    [sprite {:texture texture :x x :y y}]))

(defn tile-array
  [{:keys [x y w h id]}]
  [container {:key id :x (* x 64) :y (* y 64)}
   (for [x (range w)
         y (range h)]
     ^{:key (str x "x" y)} [tile {:x x :y y}])])

(defn game-map
  [tile-objects]
  [container {:x 0 :y 0 :scale (js/PIXI.Point. 0.9 0.9)}
   (map tile-array tile-objects)])

(defn game-root
  [world]
  [stage {:width 500 :height :300}
   [game-map (:tilemap @world)]])

;; define your app data so that it doesn't get over-written on reload
(defonce game-world (r/atom {:tilemap [{:x 0 :y 2 :w 4 :h 2 :id "ceiling"}]}))
(r/render-component [game-root game-world]
                    (. js/document (getElementById "app")))

(defn on-js-reload
  []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  #_(swap! app-state update-in [:__figwheel_counter] inc))
