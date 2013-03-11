(ns image-processing.test.nfeature-test
  (:use [clojure.test]
        [image-processing.nfeatures]
        [image-processing.charts]
        [image-processing.core]
        [image-processing.point :only [neighbour-hv? neighbour-hvd?]]))

(deftest nfeat?-test
  (is (not (nfeat? '({:x 3 :y 4} {:x 1 :y 2}))))
  (is (nfeat? '(({:x 1 :y 2} {:x 3 :y 4}) ({:x 5 :y 5} {:x 4 :y 4}))))
  (is (not (nfeat? {:a '({:x 1 :y 2} {:x 3 :y 4}) :b '({:x 5 :y 5} {:x 4 :y 4})}))))

(deftest connex-split-hv
  (is (= (split-into-connex neighbour-hv? '(({:x 1 :y 1} {:x 1 :y 2} {:x 1 :y 3} {:x 2 :y 3} {:x 3 :y 4} {:x 4 :y 4} {:x 5 :y 5} {:x 6 :y 6} {:x 8 :y 8} {:x 10 :y 10} {:x 11 :y 11})))
         '(({:y 11, :x 11})
           ({:y 10, :x 10})
           ({:y 8, :x 8})
           ({:y 6, :x 6})
           ({:y 5, :x 5})
           ({:y 4, :x 4} {:y 4, :x 3})
           ({:y 3, :x 2} {:y 3, :x 1} {:y 2, :x 1} {:y 1, :x 1}))
         )     "Spliting a feature into connex, considering neighbour pixels to be horizontal and vertical"))

(deftest connex-split-hvd
  (is (= (split-into-connex neighbour-hvd? '(({:x 1 :y 1} {:x 1 :y 2} {:x 1 :y 3} {:x 2 :y 3} {:x 3 :y 4} {:x 4 :y 4} {:x 5 :y 5} {:x 6 :y 6} {:x 8 :y 8} {:x 10 :y 10} {:x 11 :y 11})))
         '(({:y 11, :x 11} {:y 10, :x 10})
           ({:y 8, :x 8})
           ({:y 6, :x 6} {:y 5, :x 5} {:y 4, :x 4} {:y 4, :x 3} {:y 3, :x 2}  {:y 3, :x 1} {:y 2, :x 1} {:y 1, :x 1}))
         )) "Spliting a feature into connex, considering neighbour pixels to be horizontal, vertical and diagonal")

(deftest nfilter-test
  (is (= (nfilter #(< 0 %) '((1 2 3) (-1 2) (-1 3) (-1)))
         '((1 2 3) (2) (3)))))


(deftest nmap-test
  (is (= (nmap #(+ 10 %) '((1 2 3) (-1 2) (-1 3) (-1)))
         '((11 12 13) (9 12) (9 13) (9)))))

(deftest crop-test
  (is (= (crop '(({:y 14, :x 13} {:y 17, :x 16}) ({:y 17, :x 16}) ({:y 17, :x 16} {:y 24, :x 23} {:y 14, :x 13})))
         '(({:y 0, :x 0} {:y 3, :x 3}) ({:y 3, :x 3}) ({:y 3, :x 3} {:y 10, :x 10} {:y 0, :x 0})))))

(deftest apply-nfeat-to-image-test
  (is (= (apply-nfeat-to-image (image-processing.image/white-image :gray 2 2)
                               '(({:x 0 :y 0 :gray 100}) ({:x 1 :y 1 :gray 50})))
         #image_processing.image.Image{:pixels ({:gray 100} {:gray 255} {:gray 255} {:gray 50}), :width 2})))


(deftest nfeature-props-test
  (let [feat '(({:x 1 :y 2} {:x 1 :y 2} {:x 1 :y 3}) ({:x 2 :y 3} {:x 3 :y 4} {:x 4 :y 4} {:x 5 :y 5}) ({:x 6 :y 6} {:x 8 :y 8} {:x 10 :y 10} {:x 11 :y 15}))]
    (is (= (get-nfeature-max :x feat)
           11))
    (is (= (get-nfeature-min :x feat)
           1))
    (is (= (get-nfeature-max :y feat)
           15))
    (is (= (get-nfeature-min :y feat)
           2))
    (is (= (get-nfeature-width feat)
           11))
    (is (= (get-nfeature-height feat)
           14
           ))
    ))