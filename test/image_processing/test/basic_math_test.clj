(ns image-processing.test.basic-math-test
  (:use [clojure.test]
        [image-processing.basic-math]))

(deftest split-partition-test
  (let [test-set (list 1 2 3 1 2 3 4 1 2 3)
        result (list (list 4)
                     (list 3 3 3)
                     (list 2 2 2)
                     (list 1 1 1))]
    (is (= (split-by-symetrical-operator = test-set)
           result)))
  )

