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

(def img
  (-> (load-file-buffImg "test/test.jpg")
      convert-buffImg-to-image))


(defn zigzav-v-path [x]
  (map #(into {} {:x %1 :y %2}) (cycle (map #(+ x %) (range -4 5))) (range (get-height img)))
  )

(def cross-line
  (map #(into {} {:x %1 :y %2}) (range) (range (get-height img))))

(deftest path-test
  (let [img (-> (load-file-buffImg "test/test.jpg")
                convert-buffImg-to-image)
        ;; creates a vertical zig-zag-path on the 'x' coordinate
        ;; zig-zag delta are between -4 and 4
        zigzav-v-path (fn [x]
                        (map #(into {} {:x %1 :y %2}) (cycle (map #(+ x %) (range -4 5))) (range (get-height img))))
        ;; creates a path the cuts the image diagonally
        crossline (map #(into {} {:x %1 :y %2}) (range) (range (get-height img)))]
    (is (= (-> (list (feature-between-vertical-paths (zigzav-v-path 10) (zigzav-v-path 50) img)) draw-nfeat-on-blank-image)
           (load-file-Img "test/expected_path_test1.png")))
    (is (= (-> (list (feature-between-vertical-paths (zigzav-v-path 10) crossline img)) draw-nfeat-on-blank-image)
           (load-file-Img "test/expected_path_test2.png")))
    (is (= (image-between-vertical-paths (zigzav-v-path 30) (zigzav-v-path 50) img)
           (load-file-Img "test/expected_path_test3.png")))))