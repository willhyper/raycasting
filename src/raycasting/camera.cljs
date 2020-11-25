(ns raycasting.camera
  (:require-macros [raycasting.macros :refer [three-decimal] :as m]))

(def camera (atom {:x 0.0 :y 0.0 :degree 0.0}))

(defn move-forward
  "Move `camera` forward given `amount` respecting the `degree`."
  ([camera] (move-forward camera 1))
  ([camera amount]
   (let [rad (* (:degree camera) (/ Math/PI 180.0))]
     (merge-with + camera {:x (three-decimal (* (Math/sin rad) amount))
                           :y (three-decimal (* (Math/cos rad) amount))}))))


#_(comment
    (in-ns 'raycasting.camera)
    (:degree @camera)

    (swap! camera (fn [c] (assoc c :degree 40)))
    @camera
    
    (three-decimal 1.2345)
    )