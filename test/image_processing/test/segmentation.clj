(ns image-processing.test.segmentation
    (:import
      (javax.imageio ImageIO) 
      (java.io File))
    (:use 
      [image-processing.segmentation]
      [image-processing.charts]
      [image-processing.image]
      [image-processing.core])
    (:use [clojure.test]))


(def x (map #(ImageIO/read (File. %)) 
            '("test/image_processing/test/1a7r.gif" 
              "test/image_processing/test/1azc.gif")))
;(def x (ImageIO/read (File. "/home/boechat/Desktop/result.png")))

(def z (convert-buffImg-to-image (first x)))

(def y (get-binarized-values z))

(def w (vertical-histogram-autocrop y))


