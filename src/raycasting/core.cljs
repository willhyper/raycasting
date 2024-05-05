(ns raycasting.core
  (:require [raycasting.camera :as cam])
  (:require [raycasting.stage :as stage])
  (:require [raycasting.input :as input])
  (:require [raycasting.math :as math])
  (:require [raycasting.map :as map])
  (:require [raycasting.perspective :as perspective]))

(defonce ^:dynamic *canvas* (. js/document getElementById "raycaster"))
(defonce ^:dynamic *ctx* (. *canvas* getContext "2d"))
(defonce ^:dynamic *msg* (. js/document getElementById "message"))

(defn _intersect-ray [[start end :as ray] wall]
  (if (math/intersect? ray wall) 
    [start (math/intersection ray wall)] 
    ray))

(defn intersect-ray [ray walls]
  (->> walls
       (map #(_intersect-ray ray %))
       (apply min-key #(apply math/distance %))))

(defn cast-rays [camera walls]
  (let [cx (:x camera) cy (:y camera) deg (:degree camera)
        rays-angles (->> (range -10 10 1) 
                         (map #(+ deg %)) 
                         (map #(* math/radian %)))
        rays-vector (map (fn [rad] [(* 100 (Math/cos rad)) (* 100 (Math/sin rad))]) rays-angles)
        rays (map (fn [[rx ry]] [[cx cy] [(+ cx rx) (+ cy ry)]]) rays-vector)
        rays-intersected (map #(intersect-ray % walls) rays)]
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

    (when (states "ArrowLeft")
      (if (states "Shift")
        (let [{x' :x y' :y} (cam/strafe current-pos (- extra-step))]
          (when (not (math/collides? [[x y] [x' y']] stage))
            (swap! camera cam/strafe (- step-size))))
        (swap! camera cam/rotate (- rotate-angle))))

    (when (states "ArrowRight")
      (if (states "Shift")
        (let [{x' :x y' :y} (cam/strafe current-pos extra-step)]
          (when (not (math/collides?  [[x y] [x' y']] stage))
            (swap! camera cam/strafe step-size)))
        (swap! camera cam/rotate rotate-angle)))

    (when (states "ArrowUp")
      (let [{x' :x y' :y} (cam/move-forward current-pos extra-step)]
        (when (not (math/collides?  [[x y] [x' y']] stage))
          (swap! camera cam/move-forward step-size))))

    (when (states "ArrowDown")
      (let [{x' :x y' :y} (cam/move-forward current-pos (- extra-step))]
        (when (not (math/collides?  [[x y] [x' y']] stage))
          (swap! cam/camera cam/move-forward (- step-size)))))))

(defn render []
  (move-camera! cam/camera input/key-states stage/walls)
  (let
   [rays (cast-rays @cam/camera stage/walls)]

    (perspective/draw-persepctive *canvas* *ctx* rays)
    (map/draw-map *ctx* rays)    

    (. js/window requestAnimationFrame render); this is like a loop
    ))

(defn on-keydown [event]
  (let [key (.-key event)]
    (set! (. *msg* -innerHTML) key) ; update msg
    
    (input/on-key-press event) ; update key-state
    
    (render) ; update canvas
    ))

(defn ^:export init []
  (. *canvas* addEventListener "mousedown" input/on-click)
  (. *canvas* addEventListener "keyup" input/on-key-release)
  (. *canvas* addEventListener "keydown" on-keydown)
  (swap! cam/camera #(cam/set-position % 60 80 90))
  (render))

(defn stop []
  ;; stop is called before any code is reloaded
  ;; this is controlled by :before-load in the config
  (js/console.log "stop"))

