(ns tmx.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [tubax.core :as tubax]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! >!]]))

(defn log [x] (.log js/console x))

(defn ->tileset
  [obj]
  (let [{:keys [firstgid name tilewidth tileheight tilecount columns]} (:attributes obj)
        image-source (-> obj :content
                       (->> (filter #(-> % :tag (= :image))))
                       :attributes
                       :source
                       str)]
    {:firstgid (js/Number firstgid)
     :name name
     :image-source image-source
     :tilesize (map js/Number [tilewidth tileheight])
     :columns (js/Number columns)
     :rows (.ceil js/Math (/ tilecount columns))
     :tilecount (js/Number tilecount)}))

(defn ->layer
  [obj]
  (let [{:keys [width height name]} (:attributes obj)
        data (->> (:content obj)
               (#(clojure.string/split % #"[^\d]+"))
               (filter (comp not empty?))
               (map js/Number)
               (partition (js/Number width))
               (take (js/Number height))
               (map vec)
               vec)]
    {:name name
     :width (js/Number width)
     :height (js/Number height)
     :data data}))

(defn ->object
  [obj]
  (-> obj
    :attributes
    (update-in [:id] js/Number)
    (update-in [:x] js/Number)
    (update-in [:y] js/Number)
    (update-in [:width] js/Number)
    (update-in [:height] js/Number)))

(defn ->objectgroup
  [objgroup]
  (let [groupname (-> objgroup :attributes :name)]
    (for [obj (:content objgroup)]
      (-> obj
        ->object
        (assoc :group groupname)))))

(defn ->tmx
  [obj]
  (let [{:keys [width height tilewidth tileheight backgroundcolor]} (:attributes obj)
        content (:content obj)
        tilesets (->> content
                   (filter #(-> % :tag (= :tileset)))
                   (map ->tileset)
                   vec)
        layers (->> content
                 (filter #(-> % :tag (= :layer)))
                 (map ->layer)
                 vec)
        objects (->> content
                  (filter #(-> % :tag (= :objectgroup)))
                  (mapcat ->objectgroup)
                  vec)]
    {:width width
     :height height
     :tile-width tilewidth
     :tile-height tileheight
     :background-color backgroundcolor
     :tilesets tilesets
     :layers layers
     :objects objects}))

(defn url->tmx
  [url]
  (go
    (let [response (<! (http/get url))]
      (-> response
        :body
        tubax/xml->clj
        ->tmx))))
