(ns pewpew.core
  (:require [cljs.core.async :as async]
            cljsjs.react-pixi
            [reagent.core :as r]
            [taoensso.sente :as sente]))

(enable-console-print!)

(def stage (r/adapt-react-class js/ReactPIXI.Stage))
(def sprite (r/adapt-react-class js/ReactPIXI.Sprite))

(def tile-image "/img/tiles.png")

(defn tile
  [{:keys [x y]} & children]
  (into [sprite {:image tile-image :x (* x 64) :y (* y 64)}]
    children))

(defn tile-array
  [{:keys [x y w h id]}]
  (into [tile {:key id :x x :y y}]
    (for [x (range w)
          y (range h)]
      ^{:key (str x "x" y)} [tile {:x x :y y}])))

(defn game-root
  [world]
  [stage {:width 500 :height :300}
   (map tile-array (:tilemap @world))])

;; define your app data so that it doesn't get over-written on reload
(defonce game-world (r/atom {:tilemap [{:x 0 :y 2 :w 4 :h 2 :id "ceiling"}]}))
(r/render-component [game-root game-world]
                    (. js/document (getElementById "app")))

(defn on-js-reload
  []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  #_(swap! app-state update-in [:__figwheel_counter] inc))
