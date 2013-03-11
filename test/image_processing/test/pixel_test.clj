(ns image-processing.test.pixel-test
  (:use [clojure.test]
        [image-processing.pixel]))

(deftest color-distance-test
  (is (<= (- (/ (color-distance {:r 1 :g 2 :b 3} {:r 4 :g 5 :b 6}) 5.19) 1) 0.01))
  (is (= (color-distance {:gray 200} {:gray 250}) 50))
  (is (= (color-distance {:bw 0} {:bw 1}) 1)))

(deftest pix-type-test
  (is (= :argb (pix-type {:a 1 :r 2 :g 3 :b 4})))
  (is (= :gray (pix-type {:gray 250})))
  (is (= :bw (pix-type {:bw 1}))))

