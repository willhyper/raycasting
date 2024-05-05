(ns raycasting.perspective
  (:require [raycasting.math :as math]))

 (defonce ^:dynamic *inf* 100) ; ray length of 100 is treated as infinity


 (defn draw-ground [canvas ctx]
   (let [H (. canvas -height)
         W (. canvas -width)
         horizon (/ H 2)
         horizon-to-ground (- H horizon)]
     (set! (.-fillStyle ctx) "brown")
     (. ctx fillRect 0 horizon W horizon-to-ground)))

 (defn draw-sky [canvas ctx]
   (let [H (. canvas -height)
         W (. canvas -width)
         horizon (/ H 2)
         gradient (.createLinearGradient ctx 0 0 0 horizon)]
     (doto gradient
       (.addColorStop 0 "blue")
       (.addColorStop 1 "white"))
     (set! (.-fillStyle ctx) gradient)
     (. ctx fillRect 0 0 W horizon)))

 (defn reset-rect [canvas ctx]
   (let [H (. canvas -height)
         W (. canvas -width)]
     (. ctx clearRect 0 0 W H)))

 (defn draw-3d-wall
   [canvas ctx rays-colored]

   (let [H (.-height canvas)
         W (.-width canvas)

         rays (map first rays-colored)
         colors (vec (map last rays-colored))
         colors (vec (map last colors))
         
         ray-count (count rays)
         ray-width (/ W ray-count)
         horizon (/ H 2)

         ray-dists (map  (fn [[s d]] (math/distance s d))
                         rays)
         
         wall-heights (->> ray-dists
                           (map #(/ % *inf*))
                           (map #(- 1 %))
                           (map #(* H (/ % 2))))]

     (doseq [[i wall-height] (zipmap (range) wall-heights)]
       (let [wall-left-pos (* i ray-width)
             wall-width (+ 1 ray-width)
             wall-ground-pos (- horizon (/ wall-height 2))
             color (nth colors i "gray")]
         (set! (.-fillStyle ctx) color)
         (.fillRect ctx wall-left-pos wall-ground-pos wall-width wall-height)))))

 (defn draw-persepctive [canvas ctx rays]
   (reset-rect canvas ctx)
   (draw-ground canvas ctx)
   (draw-sky canvas ctx)
   (draw-3d-wall canvas ctx rays))
 

(defn dim
  "Dims two digit hex encoded color by some `amount`."
  [color amount]
  (let [dim-factor (Math/pow 1.1 (/ (inc amount) 7))
        color (int (/ color dim-factor))]
    (if (< color 25)
      25
      color)))


(defn dim-color
  "Parses six digit hex encoded color string (#000000) and returns a
  darker rgb color string."
  [color distance]
  (let [red (js/parseInt (str "0x" (subs color 1 3)))
        green (js/parseInt (str "0x" (subs color 3 5)))
        blue (js/parseInt (str "0x" (subs color 5)))]
    (str "rgb("
         (dim red distance)
         ", "
         (dim green distance)
         ", "
         (dim blue distance)
         ")")))