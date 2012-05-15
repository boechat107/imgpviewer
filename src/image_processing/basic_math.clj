(ns image-processing.basic-math
    (:import
      ;[org.apache.commons.math.stat.descriptive.moment Variance]
      [java.lang Math]))


(defn square
  "Returns the square of X."
  #^{:arglists [x]}
  [x]
  (* x x))


(defn mean
  "Returns the mean value of a sequence."
  #^{:arglists [coll]}
  [coll]
  (/ (reduce + coll) (count coll)))


(defn euclidian-distance
  "Euclidian distance between two vectors."
  [vec1 vec2]
  (Math/sqrt (reduce + (map #(square (- %1 %2))
                            (rest vec1)
                            (rest vec2)))))
