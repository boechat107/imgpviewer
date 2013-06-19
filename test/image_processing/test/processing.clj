(ns image-processing.test.processing 
  (:use 
    [clojure.test]
    [image-processing.core-new]
    )
  (:require 
    [image-processing.processing :as pr]
    [image-processing.helpers :as ih])
  )

(def img-test
  "A manually created image."
  (let [img (new-image 4 4 :rgb)
        [rch gch bch] (get-channel img)]
    (set-pixel! rch 0 255)
    (set-pixel! gch 0 255)
    (set-pixel! bch 0 255)
    (set-pixel! rch 5 255)
    (set-pixel! gch 10 255)
    (set-pixel! bch 15 255)
    img))

(defn time-test 
  []
  (let [img (time (ih/load-file-image "test/ds.jpg"))
        gray (time (pr/rgb-to-gray img)),
        bw (time (pr/binarize gray 100)),
        er (time (pr/erode bw))
        blur (time (pr/big-blur img))
        sblur (time (pr/small-blur img))
        ]
    (time (ih/view blur sblur img))
    nil))
