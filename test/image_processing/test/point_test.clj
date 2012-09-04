(ns image-processing.test.point-test
  (:use [clojure.test]
        [image-processing.point])
  (:import [image_processing.point Point]))

(deftest x-y-in-cone-test
  (let [v1 (Point. 0 0)
        v2 (Point. 1 1)]
    (is (= ((get-point-in-cone-fn v1 v2 (/ Math/PI 3.9)) (Point. 2 1)) true))
    (is (= ((get-point-in-cone-fn v1 v2 (/ Math/PI 4.1)) (Point. 2 1)) nil))
    (is (= ((get-point-in-cone-fn v1 v2 (/ Math/PI 3.9)) (Point. 1 2)) true))
    (is (= ((get-point-in-cone-fn v1 v2 (/ Math/PI 4.1)) (Point. 1 2)) nil))))

(deftest length-test
  (is (= (length (Point. 2 0)) 2.0))
  (is (= (length (Point. 0 2)) 2.0))
  (is (= (length (Point. 3 4)) 5.0)))