(ns image-processing.test.processing 
  (:use 
    [clojure.test]
    [image-processing.core-new]
    )
  (:require 
    [image-processing.processing :as pr]
    [image-processing.helpers :as ih])
  )

(defn img-test
  "A manually created image."
  (let [img (c/new-image 4 4 :rgb)
        [rch gch bch] (c/get-channel img)]
    (c/set-pixel! rch 0 255)
    (c/set-pixel! gch 0 255)
    (c/set-pixel! bch 0 255)
    (c/set-pixel! gch 5 255)
    (c/set-pixel! bch 5 255)
    (c/set-pixel! rch 10 255)
    (c/set-pixel! bch 10 255)
    (c/set-pixel! rch 15 255)
    (c/set-pixel! gch 15 255)))

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
