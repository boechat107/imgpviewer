(ns image-processing.test.processing 
  (:require 
    [image-processing.core-new :as ipc]
    [image-processing.processing :as pr]
    [image-processing.helpers :as ih]
    [image-processing.core :as oc]
    [image-processing.transformations :as tr]
    [image-processing.charts :as ch]
    [incanter.core :as ic]
    :reload-all)
  )

(defn time-test 
  []
  (let [img (time (ih/load-file-image "test/test.jpg")),
        gray (time (pr/rgb-to-gray img)),
        bw (time (pr/binarize gray 100)),
        er (time (pr/erode bw))]
    (time (ih/view img gray bw er))))

(defn time-test-two 
  []
  (let [img (time (oc/load-file-Img "test/test.jpg")),
        gray (time (oc/to-grayscale img)),
        bw (time (oc/to-binary gray 100)),
        er (time (tr/erode bw))]
    (time (->> (map oc/convert-image-to-buffImg [gray bw er])
               (apply ch/view)
               ))))
