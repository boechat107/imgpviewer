(ns image-processing.test.path-test
  (:use [clojure.test]
        [image-processing
         [path]
         [image]
         [core]
         [pixel]
         [image-feature]
         [nfeatures]
         [charts]]))

#_(def qwe
  (-> (load-file-buffImg "test/test.jpg")
      convert-buffImg-to-image))


#_(defn zigzav-v-path [x]
  (map #(into {} {:x %1 :y %2}) (cycle (map #(+ x %) (range -4 5))) (range (get-height qwe)))
  )

#_(def cross-line
  (map #(into {} {:x %1 :y %2}) (range) (range (get-height qwe))))

(deftest path-test
  (let [img (-> (load-file-buffImg "test/test.jpg")
                convert-buffImg-to-image)
        ;; creates a vertical zig-zag-path on the 'x' coordinate
        ;; zig-zag delta are between -4 and 4
        zigzav-v-path (fn [x]
                        (map #(into {} {:x %1 :y %2}) (cycle (map #(+ x %) (range -4 5))) (range (get-height qwe))))
        ;; creates a path the cuts the image diagonally
        crossline (map #(into {} {:x %1 :y %2}) (range) (range (get-height qwe)))]
    (is (.equals (-> (list (feature-between-vertical-paths (zigzav-v-path 10) (zigzav-v-path 50) qwe)) draw-nfeat-on-blank-image)
           (load-file-Img "test/expected_path_test1.png")))
    (is (.equals (-> (list (feature-between-vertical-paths (zigzav-v-path 10) crossline qwe)) draw-nfeat-on-blank-image)
           (load-file-Img "test/expected_path_test2.png")))))