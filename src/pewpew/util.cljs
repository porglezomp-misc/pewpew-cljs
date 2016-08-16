(ns pewpew.util)

(defn indexed [s] (map-indexed vector s))
(defn log [& x] (apply (.-log js/console) x))
(defn dbg [x] (doto x log))

(defn floor [x] (.floor js/Math x))
(defn ceil [x] (.ceil js/Math x))
