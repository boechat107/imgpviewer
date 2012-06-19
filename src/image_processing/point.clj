(ns image-processing.point
  (:require [clojure.contrib.math :as math]))

(defrecord point [x y])

(defn neighbour-hv? [p1 p2]
  (let [delta [(- (:x p1) (:x p2)) (- (:y p1) (:y p2))]]
    (contains? #{[-1 0] [1 0] [0 1] [0 -1]} delta)
    ))

(defn neighbour-hvd? [p1 p2]
  (let [delta [(- (:x p1) (:x p2)) (- (:y p1) (:y p2))]]
    (contains? #{[-1 0] [1 0] [0 1] [0 -1] [-1 -1] [-1 1] [1 -1] [1 1]} delta)))

(defn mid-point [v1 v2]
  (point. (/ (+ (:x v1) (:x v2)) 2)
          (/ (+ (:y v1) (:y v2)) 2)))

(defn subtract [v1 v2]
  (point. (- (:x v1) (:x v2))
          (- (:y v1) (:y v2))))

(defn length [v]
  (math/sqrt (+ (math/expt (:x v) 2) (math/expt (:y v) 2))))

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