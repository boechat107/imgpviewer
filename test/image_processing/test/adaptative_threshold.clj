(ns image-processing.test.adaptative-threshold
  (:use [clojure.test]
        [image-processing.adaptative-threshold])
  (:import [image_processing.image Image]))


(deftest integral-img-test
  (let [timg (Image. (map #(hash-map :gray %) (range 9)) 3)
        expected {[2 2] 36, [2 1] 15, [2 0] 3, [1 2] 21, [1 1] 8, [1 0] 1, [0 2] 9, [0 1] 3, [0 0] 0}]
    (is (= expected
           (integral-img timg)))))
