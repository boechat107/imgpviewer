(ns image-processing.test.core
    (:import
      (javax.imageio ImageIO) 
      (java.io File))
    (:use 
      [image-processing.charts]
      [image-processing.core])
    (:use [clojure.test]))


;(def x (map #(ImageIO/read (File. %)) 
;            '("test/image_processing/test/1a7r.gif" 
;              "test/image_processing/test/1azc.gif")))
(def x (ImageIO/read (File. "/home/boechat/Desktop/result.png")))

(def z (convert-buffImg-to-image x))

(def y (get-binarized-values z))


