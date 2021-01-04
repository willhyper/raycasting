(ns raycasting.core
  (:require [raycasting.camera :as cam])
  (:require [raycasting.stage :as stage])
  (:require [raycasting.input :as input]))


(defonce ^:dynamic *canvas* nil)
(defonce ^:dynamic *ctx* nil)
(defonce ^:dynamic *msg* nil)

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
      (apply draw-line ray))))

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

(defn clear-rect []
  (let [height (. *canvas* -height)
        width (. *canvas* -width)]
    (. *ctx* clearRect 0 0 width height)))

(defn render []
  (move-camera! cam/camera input/key-states stage/walls)
  (clear-rect)
  (draw-walls)
  (draw-camera @cam/camera)
  (cast-rays @cam/camera stage/walls)
  (. js/window requestAnimationFrame render) ; this is like a loop
  )


(defn on-keydown [event]
  (let [key (.-key event)]
    (set! (. *msg* -innerHTML) key)))

(defn ^:export init []
  (when-let [canvas (. js/document getElementById "raycaster")]
    (set! *canvas* canvas))
  (when-let [ctx (. *canvas* getContext "2d")]
    (set! *ctx* ctx))
  (when-let [msg (. js/document getElementById "message")]
    (set! *msg* msg))

  (if (or (nil? *canvas*) (nil? *ctx*) (nil? *msg*))
    (.log js/console "error! cannot find reference to *canvas*, *ctx*, or *msg*")
    (do
      (. *canvas* addEventListener "mousedown" input/on-click)
      (. *canvas* addEventListener "keydown" input/on-key-press)
      (. *canvas* addEventListener "keyup" input/on-key-release)
      (. *canvas* addEventListener "keydown" on-keydown)
      (swap! cam/camera #(cam/set-position % 60 80 90))
      (render))))

(defn stop []
  ;; stop is called before any code is reloaded
  ;; this is controlled by :before-load in the config
  (js/console.log "stop"))

#_(comment)

