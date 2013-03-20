(ns image-processing.test.point-test
  (:use [clojure.test]
        [image-processing.point])
  (:import [image_processing.point Point]))

(deftest x-y-in-cone-test
  (let [line [(Point. 0 0)
              (Point. 1 1)]]
    (is (= ((get-point-in-cone-fn line (/ Math/PI 3.9)) (Point. 2 1)) true))
    (is (= ((get-point-in-cone-fn line (/ Math/PI 4.1)) (Point. 2 1)) false))
    (is (= ((get-point-in-cone-fn line (/ Math/PI 3.9)) (Point. 1 2)) true))
    (is (= ((get-point-in-cone-fn line (/ Math/PI 4.1)) (Point. 1 2)) false))))

(deftest length-test
  (is (= (length (Point. 2 0)) 2.0))
  (is (= (length (Point. 0 2)) 2.0))
  (is (= (length (Point. 3 4)) 5.0)))

(deftest best-fit-line-test
  (is (= (best-fit-line [(Point. 1 1) (Point. 2 2)])
         [#image_processing.point.Point{:x 1, :y 1.0} #image_processing.point.Point{:x 2, :y 2.0}]))
  (is (= (best-fit-line [(Point. 1 1) (Point. 2 2) (Point. 4 4) ])
         [#image_processing.point.Point{:x 1, :y 1.0000000000000002} #image_processing.point.Point{:x 4, :y 3.9999999999999996}]))
  (is (= (best-fit-line [(Point. 1 1) (Point. 2 3) (Point. 4 4) ])
         [#image_processing.point.Point{:x 1, :y 1.4285714285714297} #image_processing.point.Point{:x 4, :y 4.214285714285714}]))
  )

(deftest dot-product-test
  (is (= (dot-product (Point. (/ (Math/sqrt 2) 2) (/ (Math/sqrt 2) 2)) (Point. 0 2)) 
         1.4142135623730951))
  (is (= (dot-product (Point. 2 2) (Point. 0 1))
         2)))

(deftest project-point-on-line-test
  (is (= (project-point-on-line [ (Point. 1 1) (Point. 3 3)]  (Point. 2 2))
         #image_processing.point.Point{:x 2.0, :y 2.0}))
  (is (= (project-point-on-line [ (Point. 0 3) (Point. 3 3)]  (Point. 2 2))
         #image_processing.point.Point{:x 2.0, :y 3.0})))

(deftest point-to-line-distance-test
  (is (= (point-to-line-distance [ (Point. 3 3) (Point. 2 2)] (Point. 1 0))
         0.7071067811865476))
  (is (= (point-to-line-distance [ (Point. 3 3) (Point. 2 2)] (Point. 0 1))
         0.7071067811865476))
  (is (= (point-to-line-distance [ (Point. 3 3) (Point. 2 2)] (Point. 4 4))
         0.0)))

(deftest x-y-in-half-line-test
  (let [line [(Point. 0 0)
              (Point. 1 1)]]
    (is (= ((get-point-in-half-line-fn line 1) (Point. 2 2)) true))
    (is (= ((get-point-in-half-line-fn line 1) (Point. 1 0)) false))
    (is (= ((get-point-in-half-line-fn line 1) (Point. 2 3)) true))
    (is (= ((get-point-in-half-line-fn line 1) (Point. 2 4)) false))))
