(ns image-processing.test.core
    (:import
      (javax.imageio ImageIO) 
      (java.io File))
    (:use [image-processing.core])
    (:use [clojure.test]))


(def x (map #(ImageIO/read (File. %)) 
            '("test/image_processing/test/1a7r.gif" 
              "test/image_processing/test/1azc.gif")))

(def z (convert-buffImg-to-image (first x)))

(def y (get-binarized-values z))
