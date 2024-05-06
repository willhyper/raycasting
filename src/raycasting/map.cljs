(ns raycasting.map
  (:require [raycasting.camera :as cam])
  (:require [raycasting.stage :as stage]))

(defn draw-line
  ([ctx [x1 y1] [x2 y2]]
   (draw-line ctx [x1 y1] [x2 y2] "#000000"))
  ([ctx [x1 y1] [x2 y2] color]
   (set! (. ctx -strokeStyle) color)
   (doto ctx
     (.beginPath)
     (.moveTo x1 y1)
     (.lineTo x2 y2)
     (.stroke)))) ; draw it https://www.w3schools.com/tags/canvas_beginpath.asp

(defn draw-camera
  "Draws `camera` on canvas, specified by `*ctx*`."
  [ctx]
  (let [camera @cam/camera
        [x y :as pos] [(:x camera) (:y camera)]
        pointer (cam/move-point pos (:degree camera) 7)
        width 10
        color "#000000"]
    (draw-line ctx pos pointer color)
    (set! (. ctx -fillStyle) color)
    (. ctx fillRect (- x (/ width 2)) (- y (/ width 2)) width width)))

(defn draw-walls [ctx]
  (doseq [wall stage/walls] 
    (apply draw-line ctx wall)))


(defn draw-rays [ctx rays-colored]  
  (doseq [ray-colored rays-colored]
    (let [ray (first ray-colored)
          color (last ray-colored)
          rsrc (first ray)
          rdst (second ray)]
      (draw-line ctx rsrc rdst color))))


(defn draw-map [ctx rays]
  (draw-walls ctx)
  (draw-camera ctx)
  (draw-rays ctx rays))