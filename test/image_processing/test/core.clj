(ns image-processing.test.core
    (:use 
      [clojure.tools.trace]
      [image-processing.transformations]
      [image-processing.segmentation]
      [image-processing.charts]
      [image-processing.image]
      [image-processing.core])
    (:use [clojure.test]))


;(def x (map #(ImageIO/read (File. %)) 
;            '("test/image_processing/test/1a7r.gif" 
;              "test/image_processing/test/1azc.gif")))
;(def x (ImageIO/read (File. "/home/boechat/Desktop/result2.png")))
;
;(def z (convert-buffImg-to-image x))
;
;(def y (to-grayscale z))
;
;(def w (to-binary z))
;
;(def k (convolve w [0.2 0.2 0.2 0.2 1.0 0.2 0.2 0.2]))

;(def k (get-subimage w 0 0 80 20))



;; (def img (-> (load-file-buffImg "test/2dveac.jpg")
;;              (convert-buffImg-to-image)
;;              (to-binary 150)
;;              (erode 0.15 0.15)
;;              (convert-image-to-buffImg)
;;              (scale-image 30 30)))

;(def s (get-subimage img 0 6 (:width img) 30))



