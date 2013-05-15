(ns image-processing.test.processing 
  (:require 
    [image-processing.core-new :as ipc]
    [image-processing.processing :as pr]
    [image-processing.helpers :as ih])
  )

(defn time-test 
  []
  (let [img (time (ih/load-file-image "test/cnh.png"))
        gray (time (pr/rgb-to-gray img)),
;        bw (time (pr/binarize gray 100)),
;        er (time (pr/erode bw))
        ]
    (time (ih/view img gray))))
