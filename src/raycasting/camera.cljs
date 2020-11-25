(ns raycasting.camera)

(def camera (atom {:x 0.0 :y 0.0 :degree 0.0}))


#_(comment
    (in-ns 'raycasting.camera)
    (:degree @camera)

    (swap! camera (fn [c] (assoc c :degree 40)))
    @camera)