(ns raycasting.core
  (:require [raycasting.camera :as cam])
  (:require [raycasting.stage :as stage])
  (:require [raycasting.input :as input])
  (:require-macros [raycasting.macros
                    :refer [three-decimal]
                    :as m]))



(defonce ^:dynamic *canvas* nil)
(defonce ^:dynamic *ctx* nil)
(defonce ^:dynamic *msg* nil)

(defonce ^:dynamic *ray-count* 42)
(defonce ^:dynamic *fov* 60)
(defonce ^:dynamic *compensate-fisheye* true)

(def max-fov 360)
(def max-compensated-fov 180)
(def infinity 100000)


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


(defn v- [[px py] [qx qy]]
  [(- qx px) (- qy py)])
(defn v+ [[px py] [qx qy]]
  [(+ qx px) (+ qy py)])
(defn v* [[px py] s]
  [(* s px) (* s py)])
(defn distance [[px py] [qx qy]]
  (let [dx (- px qx)
        dy (- py qy)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn outer-product
  ([[px py] [qx qy]] (- (* px qy) (* py qx)))
  ([O A B] (outer-product (v- O A) (v- O B))))

(defn ccw? [O A B] (pos? (outer-product O A B)))
(defn cw?  [O A B] (neg? (outer-product O A B)))
(defn colinear? [O A B] (= 0 (outer-product O A B)))


(defn convex-quadrilateral?
  {:test #(let [[A B C D] [[1 0] [-1 0] [0 1] [0 -1]]]
            (assert (convex-quadrilateral? A C B D)))}
  [P Q R S]
  (let [angles (map #(apply outer-product %) [[P Q R] [Q R S] [R S P] [S P Q]])
        non-colinears (filter #(not= 0 %) angles)]
    (or (every? pos? non-colinears)
        (every? neg? non-colinears))))


(defn intersect?
  {:test #(let [[A B C D] [[1 0] [-1 0] [0 1] [0 -1]]]
            (assert (intersect? [A B] [C D])))}
  [[A B] [C D]]
  (convex-quadrilateral? A C B D))

(defn collides?
  "Check if `path` intersects with any `stage` walls."
  [ray walls]
  (some true? (map #(intersect? % ray) walls)))

(defn intersection
  {:test #(do (assert (= [0.5 0.5] (intersection [[0 0] [1 1]] [[0 1] [1 0]]))))}
  [[A B] [C D]]
  (let [n (outer-product (v- C D) (v- A C))
        d (outer-product (v- C D) (v- A B))
        t (/ n d)]
    (v+ (v* B t) (v* A (- 1 t)))))


(defn _intersect-ray [[start end :as ray] wall]
  (if (intersect? ray wall) [start (intersection ray wall)] ray))

(defn intersect-ray [ray walls]
  (->> walls (map #(_intersect-ray ray %)) (apply min-key #(apply distance %))))

(defn cast-rays [camera walls]
  (let [cx (:x camera) cy (:y camera) deg (:degree camera)
        rays-angles (->> (range -10 10 2) (map #(+ deg %)) (map #(* cam/radian %)))
        rays-vector (map (fn [rad] [(* 100 (Math/cos rad)) (* 100 (Math/sin rad))]) rays-angles)
        rays (map (fn [[rx ry]] [[cx cy] [(+ cx rx) (+ cy ry)]]) rays-vector)
        rays-intersected (map #(intersect-ray % walls) rays)]

    (doseq [ray rays-intersected]
      (apply draw-line ray))
    rays-intersected))

(defn move-camera!
  "Handles `camera` movement within `stage`. Accepts `camera` and
  `key-states` atoms."
  [camera key-states stage]
  (let [{x :x y :y :as current-pos} @camera
        states @key-states
        step-size 0.8
        rotate-angle 1.5
        extra-step (+ step-size 1.5)]

    (when (states "Escape")
      (input/release-focus)
      (reset! key-states {}))

    (when (states "ArrowRight")
      (if (states "Shift")
        (let [{x' :x y' :y} (cam/strafe current-pos (- extra-step))]
          (when (not (collides? [[x y] [x' y']] stage))
            (swap! camera cam/strafe (- step-size))))
        (swap! camera cam/rotate (- rotate-angle))))

    (when (states "ArrowLeft")
      (if (states "Shift")
        (let [{x' :x y' :y} (cam/strafe current-pos extra-step)]
          (when (not (collides?  [[x y] [x' y']] stage))
            (swap! camera cam/strafe step-size)))
        (swap! camera cam/rotate rotate-angle)))

    (when (states "ArrowUp")
      (let [{x' :x y' :y} (cam/move-forward current-pos extra-step)]
        (when (not (collides?  [[x y] [x' y']] stage))
          (swap! camera cam/move-forward step-size))))

    (when (states "ArrowDown")
      (let [{x' :x y' :y} (cam/move-forward current-pos (- extra-step))]
        (when (not (collides?  [[x y] [x' y']] stage))
          (swap! cam/camera cam/move-forward (- step-size)))))))

(defn reset-rect []
  (let [H (. *canvas* -height)
        W (. *canvas* -width)
        horizon (/ H 2)
        horizon-to-ground (- H horizon)]
    (. *ctx* clearRect 0 0 W H)
    
    (set! (.-fillStyle *ctx*) "lightblue")
    (. *ctx* fillRect 0 0 W horizon)

    (set! (.-fillStyle *ctx*) "brown")
    (. *ctx* fillRect 0 horizon W horizon-to-ground)))

(defn projection-distance
  "Calculate projection distance between player and projection plane."
  []
  (three-decimal
   (/ (/ *ray-count* 2) (Math/atan (* (/ *fov* 2) cam/radian)))))

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



(defn draw-3d-wall
  "Draws pseudo 3d stage on `*canvas*`."
  [rays]

  (let [H (.-height *canvas*)
        W (.-width *canvas*)
        ray-count (count rays)
        ray-width (/ W ray-count)
        horizon (/ H 2)
        
        ray-dists (map  (fn [[s d]] (distance s d))
                        rays)
        wall-heights ray-dists]
    
    (set! (.-fillStyle *ctx*) "gray")

    (doseq [[i wall-height] (zipmap (range) wall-heights)]
      (let [wall-left-pos (* i ray-width)
            wall-width ray-width
            wall-ground-pos (- horizon (/ wall-height 2))]
        (.fillRect *ctx* wall-left-pos wall-ground-pos wall-width wall-height)))))


(comment)

(defn render []
  (let
   [rays (cast-rays @cam/camera stage/walls)]

    (reset-rect)
    (draw-3d-wall rays)

    (move-camera! cam/camera input/key-states stage/walls)

    (draw-walls)
    (draw-camera @cam/camera)
    (cast-rays @cam/camera stage/walls)
    (. js/window requestAnimationFrame render); this is like a loop
    ))

(defn on-keydown [event]
  (let [key (.-key event)]
    (set! (. *msg* -innerHTML) key) ; update msg
    (move-camera! cam/camera input/key-states stage/walls) ; update camera
    (input/on-key-press event) ; update key-state
    (render) ; update canvas
    ))

(defn ^:export init []
  (if-let [canvas (. js/document getElementById "raycaster")]
    (set! *canvas* canvas) (throw {:message "cannot get canvas"}))
  (if-let [ctx (. *canvas* getContext "2d")]
    (set! *ctx* ctx) (throw {:message "cannot get context"}))
  (if-let [msg (. js/document getElementById "message")]
    (set! *msg* msg) (throw {:message "cannot get message"}))

  (. *canvas* addEventListener "mousedown" input/on-click)
  (. *canvas* addEventListener "keyup" input/on-key-release)
  (. *canvas* addEventListener "keydown" on-keydown)
  (swap! cam/camera #(cam/set-position % 60 80 90))
  (render))

(defn stop []
  ;; stop is called before any code is reloaded
  ;; this is controlled by :before-load in the config
  (js/console.log "stop"))

#_(comment)

