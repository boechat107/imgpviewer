(ns image-processing.test.ellipse-finder
  (:use [clojure.test]
        [image-processing.ellipse-finder :only [find-ellipse]]))

(deftest ellipse-sample
  (is (= (find-ellipse [{:x 0 :y 0} {:x 2 :y 0} {:x 1 :y 1} {:x 1 :y -1} {:x 1 :y 2} {:x 1 :y -2} {:x 1.1 :y -2} {:x 1.2 :y -1.9} {:x -1 :y -1} {:x -10 :y -10}] [1 3] [1 3])
         [4 {:y 0, :x 0} {:y 0, :x 2} 2]) "Finding a simple ellipse with 4 pixels major axis=, minor axis=1"))


;; (deftest ellipse-from-image)