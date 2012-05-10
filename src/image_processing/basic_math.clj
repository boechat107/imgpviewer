(ns image-processing.basic-math
    (:import
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
