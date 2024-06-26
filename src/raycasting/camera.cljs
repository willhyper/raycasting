(ns raycasting.camera
  (:require-macros [raycasting.macros :refer [three-decimal] :as m]))

(def camera (atom {:x 0.0 :y 0.0 :degree 0.0}))
(def radian (/ Math/PI 180.0))

(defn move-point [[x y] degree amount]
  (let [angle (* degree radian)]
    [(+ x (three-decimal (* amount (Math/cos angle))))
     (+ y (three-decimal (* amount (Math/sin angle))))]))

(defn move-forward
  "Move `camera` forward given `amount` respecting the `degree`."
  ([camera] (move-forward camera 1))
  ([camera amount]
   (let [{x :x
          y :y
          degree :degree} camera
         [x y] (move-point [x y] degree amount)]
     (assoc camera  :x x :y y))))


(defn rotate
  ([camera] (rotate camera 0.5))
  ([camera degree] 
   (let [deg (-> (:degree camera) (+ degree) (mod 360.0))]
     (assoc camera :degree deg)))
  )

(defn set-position
  ([cam] (set-position cam 0 0 0))
  ([cam x y] (set-position cam x y 0))
  ([cam x y deg] (assoc cam :x x :y y :degree deg))
)

(defn strafe
  "Move `camera` to the side given `amount` respecting the `degree`."
  ([camera] (move-forward camera 1))
  ([camera amount]
   (let [{x :x
          y :y
          degree :degree} camera
         [x y] (move-point [x y] (mod (+ degree 90) 360.0) amount)]
     (assoc camera :x x :y y))))

#_(comment
    (:degree @camera)

    (swap! camera (fn [c] (assoc c :degree 40)))
    @camera

    (three-decimal 1.2345)

    (move-point [0 0] 45 1)

    (swap! camera move-forward)
    
    (rotate @camera)
    (swap! camera rotate)
    
    )