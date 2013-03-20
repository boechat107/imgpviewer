(ns image-processing.point
  (:require [image-processing.basic-math :as bmath]))


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x# "!\n") x#))

(defrecord Point [x y])

(defn neighbour-hv? [p1 p2]
  (let [delta [(- (:x p1) (:x p2)) (- (:y p1) (:y p2))]]
    (contains? #{[-1 0] [1 0] [0 1] [0 -1]} delta)
    ))

(defn neighbour-hvd? [p1 p2]
  (let [delta [(- (:x p1) (:x p2)) (- (:y p1) (:y p2))]]
    (contains? #{[-1 0] [1 0] [0 1] [0 -1] [-1 -1] [-1 1] [1 -1] [1 1]} delta)))

(defn neighbour-v? [p1 p2]
  (let [delta [(- (:x p1) (:x p2)) (- (:y p1) (:y p2))]]
    (contains? #{[0 1] [0 -1]} delta)
    ))

(defn neighbour-h? [p1 p2]
  (let [delta [(- (:x p1) (:x p2)) (- (:y p1) (:y p2))]]
    (contains? #{[-1 0] [1 0]} delta)
    ))

(defn neighbour-hd? [p1 p2]
  (let [delta [(- (:x p1) (:x p2)) (- (:y p1) (:y p2))]]
    (contains? #{[-1 0] [1 0] [-1 -1] [-1 1] [1 -1] [1 1]} delta)
    ))

(defn mid-point [v1 v2]
  (Point. (/ (+ (:x v1) (:x v2)) 2)
          (/ (+ (:y v1) (:y v2)) 2)))

(defn subtract [v1 v2]
  (Point. (- (:x v1) (:x v2))
          (- (:y v1) (:y v2))))

(defn add [v1 v2]
  (Point. (+ (:x v1) (:x v2))
          (+ (:y v1) (:y v2))))

(defn length [v]
  (Math/sqrt (+ (Math/pow (:x v) 2) (Math/pow (:y v) 2))))

(defn distance [v1 v2]
  (length (subtract v1 v2)))

(defn dot-product [v1 v2]
  (+ (* (:x v1) (:x v2)) (* (:y v1) (:y v2))))

(defn angle [v1 v2]
  (let [l-v1 (length v1)
        l-v2 (length v2)]
    (if (and (not= l-v1 0)
             (not= l-v2 0))
      (Math/acos (/ (+ (* (:x v1) (:x v2))
                       (* (:y v1) (:y v2)))
                    (length v1)
                    (length v2)))
      0)))


(defn project-point-on-line
  "Return the closest line coordinate from the point.
   The line is defined by a pair of points [P1 P2]"
  [line p]
  
  (let [[u v] line
        alpha (/ (+ (* (- (:x v) (:x p)) (- (:x v) (:x u)))
                    (* (- (:y v) (:y p)) (- (:y v) (:y u))))
                 (+ (Math/pow (- (:x v) (:x u)) 2)
                    (Math/pow (- (:y v) (:y u)) 2)))
        compl-alpha (- 1 alpha)]
    (Point. (+ (* alpha (:x u)) (* compl-alpha (:x v)))
            (+ (* alpha (:y u)) (* compl-alpha (:y v))))))

(defn point-to-line-distance [line point]
    (length (subtract point (project-point-on-line line point))))

(defn get-point-in-half-line-fn
  "This function returns a function that test if a given x,y is inside a half-line tolerance. The half line is given by x1,y1->x2,y2->Inf and the tolerance is given by the distance to the half line
                    .
   tolerance    . '
   region   . '     * x2-x1,x2-y1 half line
        . '     *       . 
    . '     *   X   . '
        *       . ' 
    O       . '  x,y to be tested        
Origin  . '
x1,y2 

   The algorithm simply check if the distance to the from the point to the line is less than the tolerance and if the angle is less than 90 - or the dot product is positive
"
  [line tolerance]
  (let [[v1 v2] line
        v2-coord (subtract v2 v1)]
    (fn [p]
      (let [p-coord (subtract p v2)]
        (if (and (< 0 (dot-product v2-coord p-coord))
                 (<= (point-to-line-distance line p) tolerance))
          true
          false)))))


(defn get-point-in-cone-fn
  "This function returns a function that test if a given x,y lies inside a cone.  The cone origin is x2,y2 and the vector passing in the center of the cone is x2-x1,y2-y1 - as ilustrated

        /            * x2-x1, y2-y1 -> vector passin on the cone center
       /,theta   * 
      /     *                  . x,y to be tested
     /  *     theta        
    O _______/____
Origin x2,y2 

   The algorithm simply put the x,y in the cone coordinate system
   and check if the angle between the center vector is less than theta
"
  [line theta]
  (let [[v1 v2] line
        cone-center-vector (Point. (- (:x v2) (:x v1)) (- (:y v2) (:y v1)))]
    (fn [v]
      (let [v-cone-coords (subtract v v2)
            theta-from-center (angle cone-center-vector v-cone-coords)]
        (if (<= theta-from-center theta)
          true
          false)
        ))))


(defn best-fit-line
  "Returns best fit line that passes by the list of points.
   The calculations were taken from
      http://hotmath.com/hotmath_help/topics/line-of-best-fit.html

   The return are two vectors by which the line passes. These vectors are
      min-x,f min-x
      max-x, f max-x

      where f is the best fit line function the x-y value of the line on the min and max"
  [points]
  (let [xs (map :x points)
        ys (map :y points)
        sumx (apply + xs)
        sumy (apply + ys)
        sumxy (apply + (map * xs ys))
        sumxsq (apply + (map #(Math/pow % 2) xs))
        n (count points)
        m (/ (- sumxy (/ (* sumx sumy) n)) (- sumxsq (/ (Math/pow sumx 2) n)))
        
        meanx (bmath/mean xs)
        meany (bmath/mean ys)
        b (- meany (* m meanx))

        y-at-x (fn [x] (+ (* m x) b))
        min-x (apply min xs)
        max-x (apply max xs)
        ]
    [(Point. min-x (y-at-x min-x)) (Point. max-x (y-at-x max-x))]))

