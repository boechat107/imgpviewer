(ns image-processing.test.image
  (:use [clojure.test])
  (:use [image-processing.image])
  (:import [image_processing.image Image]))

(deftest select-point
  (let [mimg (Image. (vec (range 20)) 4)]
    (is 11 (get-point mimg 3 2))
    (is nil? (get-point mimg 4 2))))
