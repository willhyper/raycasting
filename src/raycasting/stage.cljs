(ns raycasting.stage)

(def X 600)
(def Y 200)
(defn make-a-color
  []
  (apply str "#" (repeatedly 6 #(rand-nth (vec "0123456789abcdef")))))

(defn make-a-wall
  []
  [[(rand-int X) (rand-int Y)] [(rand-int X) (rand-int Y)] (make-a-color)])

(defn make-a-box
  []
  (let [x (rand-int X)
        y (rand-int Y)
        w 10
        h 10]
    [ [[x y] [x (+ h y)] "#ff0000"]
     [[x y] [(+ x w) y] "#ff0000"]
     [[x (+ h y)] [(+ w x) (+ h y)] "#ff0000"]
     [[(+ w x) y] [(+ w x) (+ h y)] "#ff0000"]]
    )
  
  )
(def walls
  (concat (make-a-box) (repeatedly 10 #(make-a-wall)))
  )



