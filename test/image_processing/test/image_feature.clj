(ns image-processing.test.image-feature
  (:use [clojure.test]
        [image-processing.charts]
        [image-processing.image-feature]
        [image-processing.point :only [neighbour-hv? neighbour-hvd?]])
 )

(deftest connex-split-hv
  (is (=
       (split-feature-into-connex neighbour-hv? [{:x 1 :y 1} {:x 1 :y 2} {:x 1 :y 3} {:x 2 :y 3} {:x 3 :y 4} {:x 4 :y 4} {:x 5 :y 5} {:x 6 :y 6} {:x 8 :y 8} {:x 10 :y 10} {:x 11 :y 11}])
       [[{:y 1, :x 1} {:y 2, :x 1} {:y 3, :x 1} {:y 3, :x 2}] [{:y 4, :x 3} {:y 4, :x 4}] [{:y 5, :x 5}] [{:y 6, :x 6}] [{:y 8, :x 8}] [{:y 10, :x 10}] [{:y 11, :x 11}]])
      "Spliting a feature into connex, considering neighbour pixels to be horizontal and vertical"))

(deftest connex-split-hvd
  (is (=
       (split-feature-into-connex neighbour-hvd? [{:x 1 :y 1} {:x 1 :y 2} {:x 1 :y 3} {:x 2 :y 3} {:x 3 :y 4} {:x 4 :y 4} {:x 5 :y 5} {:x 6 :y 6} {:x 8 :y 8} {:x 10 :y 10} {:x 11 :y 11}])
       [[{:y 1, :x 1} {:y 2, :x 1} {:y 3, :x 1} {:y 3, :x 2} {:y 4, :x 3} {:y 4, :x 4} {:y 5, :x 5} {:y 6, :x 6}] [{:y 8, :x 8}] [{:y 10, :x 10} {:y 11, :x 11}]])
      ) "Spliting a feature into connex, considering neighbour pixels to be horizontal, vertical and diagonal")

(deftest paint-feature-test
  (is (= (paint-feature {:r 1 :g 5 :b 7} [{:x 1 :y 1} {:x 1 :y 2} {:x 1 :y 3} {:x 2 :y 3} {:x 3 :y 4} {:x 4 :y 4} {:x 5 :y 5} {:x 6 :y 6} {:x 8 :y 8} {:x 10 :y 10} {:x 11 :y 11}])
      [{:x 1 :y 1 :r 1 :g 5 :b 7} {:x 1 :y 2 :r 1 :g 5 :b 7} {:x 1 :y 3 :r 1 :g 5 :b 7} {:x 2 :y 3 :r 1 :g 5 :b 7} {:x 3 :y 4 :r 1 :g 5 :b 7} {:x 4 :y 4 :r 1 :g 5 :b 7} {:x 5 :y 5 :r 1 :g 5 :b 7} {:x 6 :y 6 :r 1 :g 5 :b 7} {:x 8 :y 8 :r 1 :g 5 :b 7} {:x 10 :y 10 :r 1 :g 5 :b 7} {:x 11 :y 11 :r 1 :g 5 :b 7}]))
  )


(deftest feature-props-test
  (let [feat [{:x 1 :y 2} {:x 1 :y 2} {:x 1 :y 3} {:x 2 :y 3} {:x 3 :y 4} {:x 4 :y 4} {:x 5 :y 5} {:x 6 :y 6} {:x 8 :y 8} {:x 10 :y 10} {:x 11 :y 15}]]
    (is (= (get-feature-max :x feat)
           11))
    (is (= (get-feature-min :x feat)
           1))
    (is (= (get-feature-max :y feat)
           15))
    (is (= (get-feature-min :y feat)
           2))
    (is (= (get-feature-width feat)
           11))
    (is (= (get-feature-height feat)
           14
           ))
    ))