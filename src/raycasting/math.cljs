(ns raycasting.math)

(def radian (/ Math/PI 180.0))

(defn v- [[px py] [qx qy]]
  [(- qx px) (- qy py)])
(defn v+ [[px py] [qx qy]]
  [(+ qx px) (+ qy py)])
(defn v* [[px py] s]
  [(* s px) (* s py)])
(defn distance [[px py] [qx qy]]
  (let [dx (- qx px)
        dy (- qy py)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))
(defn angle [[px py] [qx qy]]
  (let [dx (- qx px)
        dy (- qy py)]
    (/ (Math/atan (/ dx dy)) radian)))

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

(defn middle 
  {:test #(do (assert (= 3 (middle [0 1 2 3 4 5]))))}
  [lst]
  (let [n (count lst)]
    (nth lst (quot n 2))))
