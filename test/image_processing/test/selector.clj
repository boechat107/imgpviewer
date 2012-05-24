(ns image-processing.test.selector
  (:use [image-processing.selector])
  (:use [clojure.test]))

(defn selector-test
  "This should print true every time you click ok."
  []
  (see-component (selector {"oi"  [190 "something"]
                            "ola" [200 "xx"]
                            "wee" [200 "xx"]}
                           (fn [x] (println (= x [190 200 200]))))))


