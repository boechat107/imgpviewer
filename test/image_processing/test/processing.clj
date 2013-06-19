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
        [rch gch bch] (get-channel img)
        fname "test/img_test.png"]
    (set-pixel! rch 0 255)
    (set-pixel! gch 0 255)
    (set-pixel! bch 0 255)
    (set-pixel! rch 5 255)
    (set-pixel! gch 10 255)
    (set-pixel! bch 15 255)
    (ih/save-to-file! img fname)
    (ih/load-file-image fname)))

(deftest pixel-vals
  (let [rch (get-channel img-test 0)
        gch (get-channel img-test 1)
        bch (get-channel img-test 2)]
    (is (== 255 (get-pixel rch 0)))
    (is (== 255 (get-pixel gch 0)))
    (is (== 255 (get-pixel bch 0)))
    (is (== 255 (get-pixel rch 5)))
    (is (== 0 (get-pixel gch 5)))
    (is (== 0 (get-pixel bch 5)))
    (is (== 0 (get-pixel rch 10)))
    (is (== 255 (get-pixel gch 10)))
    (is (== 0 (get-pixel bch 10)))
    (is (== 0 (get-pixel rch 15)))
    (is (== 0 (get-pixel gch 15)))
    (is (== 255 (get-pixel bch 15)))))

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
