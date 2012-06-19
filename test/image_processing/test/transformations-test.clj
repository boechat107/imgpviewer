(ns image-processing.test.transformations-test
  (:use [clojure.test]
        [image-processing.transformations])
  (:import [image_processing.image Image]))

(deftest grayscale-test
  (is (= (image-processing.transformations/grayscale (Image. [{:r 30 :g 30 :b 30} {:r 40 :g 40 :b 40} {:r 50 :g 50 :b 50} {:r 60 :g 60 :b 60}] 2))
         #image_processing.image.Image{:pixels ({:gray 30} {:gray 40} {:gray 50} {:gray 60}), :width 2}))
  (is (= (image-processing.transformations/grayscale (Image. [{:gray 20} {:gray 30} {:gray 40} {:gray 50}] 2))
         #image_processing.image.Image{:pixels [{:gray 20} {:gray 30} {:gray 40} {:gray 50}], :width 2})))

(deftest bw-test
  (is (= (bw 35 (Image. [{:gray 20} {:gray 30} {:gray 40} {:gray 50}] 2))
         #image_processing.image.Image{:pixels ({:bw 0} {:bw 0} {:bw 1} {:bw 1}), :width 2}))
  (is (= (bw 35 (Image. [{:r 30 :g 30 :b 30} {:r 40 :g 40 :b 40} {:r 50 :g 50 :b 50} {:r 60 :g 60 :b 60}] 2))
         #image_processing.image.Image{:pixels ({:bw 0} {:bw 1} {:bw 1} {:bw 1}), :width 2})))