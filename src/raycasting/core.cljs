(ns raycasting.core
  (:require [raycasting.camera :as cam])
  (:require [raycasting.stage :as stage]))


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

(defn draw-walls []
  (doseq [wall stage/walls] (apply draw-line wall)))


(def O [0 0])
(def A [1 0])
(def B [-1 0])
(def C [0 1])
(def D [0 -1])

(defn v- [[px py] [qx qy]]
  [(- qx px) (- qy py)])
(defn v+ [[px py] [qx qy]]
  [(+ qx px) (+ qy py)])
(defn v* [[px py] s]
  [(* s px) (* s py)])

(defn outer-product
  ([[px py] [qx qy]] (- (* px qy) (* py qx)))
  ([O A B] (outer-product (v- O A) (v- O B))))

(defn ccw? [O A B] (pos? (outer-product O A B)))
(defn cw?  [O A B] (neg? (outer-product O A B)))
(defn colinear? [O A B] (= 0 (outer-product O A B)))


(defn convex-quadrilateral? [P Q R S]
  (let [a0 (outer-product P Q R)
        a1 (outer-product Q R S)
        a2 (outer-product R S P)
        a3 (outer-product S P Q)
        non-colinears (filter #(not= 0 %) [a0 a1 a2 a3])]
    (if (every? pos? non-colinears) true
        (every? neg? non-colinears))))

(convex-quadrilateral? A C B D)

(defn intersect? [[A B] [C D]]
  (convex-quadrilateral? A C B D))

(intersect? [A B] [C D])

(defn intersection
  {:test #(do (assert (= [0.5 0.5] (intersection [[0 0] [1 1]] [[0 1] [1 0]]))))}
  [[A B] [C D]]
  (let [n (outer-product (v- C D) (v- A C))
        d (outer-product (v- C D) (v- A B))
        t (/ n d)]
    (v+ (v* B t) (v* A (- 1 t)))))



#_(comment
    (draw-line [0 10] [200 40])

    @cam/camera
    (draw-camera @cam/camera)

    (draw-walls)

    (swap! cam/camera #(cam/set-position % 100 120 180))
    )

