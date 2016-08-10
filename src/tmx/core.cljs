(ns tmx.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [tubax.core :as tubax]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! >!]]))

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
                 vec)]
    {:tilesets tilesets
     :layers layers}))

(defn url->tmx
  [url]
  (go
    (let [response (<! (http/get url))]
      (-> response
        :body
        tubax/xml->clj
        ->tmx))))
