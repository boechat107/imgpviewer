(ns image-processing.pixel)

(defn neighbour-hv? [pixel1 pixel2]
  (let [delta [(- (:x pixel1) (:x pixel2)) (- (:y pixel1) (:y pixel2))]]
    (contains? #{[-1 0] [1 0] [0 1] [0 -1]} delta)
    ))

(defn neighbour-hvd? [pixel1 pixel2]
  (let [delta [(- (:x pixel1) (:x pixel2)) (- (:y pixel1) (:y pixel2))]]
    (contains? #{[-1 0] [1 0] [0 1] [0 -1] [-1 -1] [-1 1] [1 -1] [1 1]} delta)))