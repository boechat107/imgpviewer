(ns image-processing.test.nfeatures
  (:use [clojure.test]
        [image-processing.nfeatures]
        [image-processing.pixel :only [neighbour-hv? neighbour-hvd?]]))

(deftest type-test
  (is (= (feat-type? [{:x 1 :y 2} {:x 3 :y 4}])
         :feat))
  (is (= (feat-type? [[ {:x 1 :y 2} {:x 3 :y 4}] [ {:x 5 :y 5} {:x 4 :y 4}]])
         :nfeat))
  (is (= (feat-type? { :a [{:x 1 :y 2} {:x 3 :y 4}] :b [ {:x 5 :y 5} {:x 4 :y 4}]})
         :map-nfeat)))

(deftest connex-split-hv
  (is (=
       (split-into-connex neighbour-hv? [{:x 1 :y 1} {:x 1 :y 2} {:x 1 :y 3} {:x 2 :y 3} {:x 3 :y 4} {:x 4 :y 4} {:x 5 :y 5} {:x 6 :y 6} {:x 8 :y 8} {:x 10 :y 10} {:x 11 :y 11}])
       [[{:y 1, :x 1} {:y 2, :x 1} {:y 3, :x 1} {:y 3, :x 2}] [{:y 4, :x 3} {:y 4, :x 4}] [{:y 5, :x 5}] [{:y 6, :x 6}] [{:y 8, :x 8}] [{:y 10, :x 10}] [{:y 11, :x 11}]])
      "Spliting a feature into connex, considering neighbour pixels to be horizontal and vertical"))

(deftest connex-split-hvd
  (is (=
       (split-into-connex neighbour-hvd? [{:x 1 :y 1} {:x 1 :y 2} {:x 1 :y 3} {:x 2 :y 3} {:x 3 :y 4} {:x 4 :y 4} {:x 5 :y 5} {:x 6 :y 6} {:x 8 :y 8} {:x 10 :y 10} {:x 11 :y 11}])
       [[{:y 1, :x 1} {:y 2, :x 1} {:y 3, :x 1} {:y 3, :x 2} {:y 4, :x 3} {:y 4, :x 4} {:y 5, :x 5} {:y 6, :x 6}] [{:y 8, :x 8}] [{:y 10, :x 10} {:y 11, :x 11}]])
) "Spliting a feature into connex, considering neighbour pixels to be horizontal, vertical and diagonal")










