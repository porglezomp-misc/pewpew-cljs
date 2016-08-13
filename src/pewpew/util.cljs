(ns pewpew.util)

(defn indexed [s] (map-indexed vector s))
(defn log [& x] (apply (.-log js/console) x))
(defn dbg [x] (doto x log))

(defn box-component
  [{:keys [box style]}]
  (when-let [[x0 y0 x1 y1] box]
    (let [w (- x1 x0)
          h (- y1 y0)
          box-style {:left x0 :top y0
                     :width w :height h
                     :position "absolute"
                     :background-color "transparent"
                     :border-radius 3
                     :border "solid 1px #44a"
                     :box-sizing "border-box"}]
      [:div {:style (merge box-style style)}])))

(defn boxes-component
  [{:keys [width height]} & boxes]
  [:div {:style {:width width :height height :position "relative"}}
   (for [[i b] (indexed boxes)]
     ^{:key i} [box-component b])])
