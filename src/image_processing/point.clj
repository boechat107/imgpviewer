(ns image-processing.point)

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

(defn mid-point [v1 v2]
  (Point. (/ (+ (:x v1) (:x v2)) 2)
          (/ (+ (:y v1) (:y v2)) 2)))

(defn subtract [v1 v2]
  (Point. (- (:x v1) (:x v2))
          (- (:y v1) (:y v2))))

(defn length [v]
  (Math/sqrt (+ (Math/pow (:x v) 2) (Math/pow (:y v) 2))))

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


(defn get-point-in-cone-fn
  "This function returns a function that test if a given (x,y) lies inside a cone.  The cone origin is (x2,y2) and the vector passing in the center of the cone is (x2-x1,y2-y1) - as ilustrated

        /            * (x2-x1, y2-y1) -> vector passin on the cone center
       /,theta   * 
      /  )   *                  . (x,y) to be tested
     /  *     )theta        
    O _______/____
Origin (x2,y2) 

   The algorithm simply put the (x,y) in the cone coordinate system
   and check if the angle between the center vector is less than theta
"
  [v1 v2 theta]
  (let [cone-center-vector (Point. (- (:x v2) (:x v1)) (- (:y v2) (:y v1)))]
    (fn [v]
      (let [v-cone-coords (subtract v v2)
            theta-from-center (angle cone-center-vector v-cone-coords)]
        (if (<= theta-from-center theta)
          true)
))))