(ns raycasting.core
  (:require [raycasting.camera :as cam]))


(defonce ^:dynamic *canvas* nil)
(defonce ^:dynamic *ctx* nil)

(defn ^:export init []
  (when-let [canvas (. js/document getElementById "raycaster")]
    (set! *canvas* canvas)
    (when (. *canvas* -getContext)
      (set! *ctx* (. *canvas* getContext "2d"))
      (set! (. *ctx* -fillStyle) "yellow")
      (. *ctx* fillRect 0 0 512 512))))

(defn stop []
  ;; stop is called before any code is reloaded
  ;; this is controlled by :before-load in the config
  (js/console.log "stop"))


(defn draw-line
  ([[x1 y1] [x2 y2]]
   (draw-line [x1 y1] [x2 y2] "#000000"))
  ([[x1 y1] [x2 y2] color]
   (set! (. *ctx* -strokeStyle) color)
   (doto *ctx*
     (.beginPath)
     (.moveTo x1 y1)
     (.lineTo x2 y2)
     (.stroke)))) ; draw it https://www.w3schools.com/tags/canvas_beginpath.asp



(defn draw-camera
  "Draws `camera` on canvas, specified by `*ctx*`."
  [camera]
  (let [[x y :as pos] [(:x camera) (:y camera)]
        pointer (cam/move-point pos (:degree camera) 7)
        width 10
        color "#000000"]
    (draw-line pos pointer color)
    (set! (. *ctx* -fillStyle) color)
    (. *ctx* fillRect (- x (/ width 2)) (- y (/ width 2)) width width)))

#_(comment
    (draw-line [0 10] [200 40])

    @cam/camera    
    (draw-camera @cam/camera)

    )

