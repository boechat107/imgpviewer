(ns image-processing.test.processing 
  (:require 
    [image-processing.core-new :as ipc]
    [image-processing.processing :as pr]
    [image-processing.helpers :as ih]
    [mikera.image.core :as mc]
    [mikera.image.filters :as mf]
    )
  (:import 
    [java.awt.image BufferedImage]
    )
  )

(defn time-test 
  []
  (let [img (time (ih/load-file-image "test/ds.jpg")),
        gray (time (pr/rgb-to-gray img)),
        bw (time (pr/binarize gray 100)),
        er (time (pr/erode bw))]
    nil
    ;(time (ih/view img gray bw er))
    ))

(defn imagez-test
  []
  (let [img (time (mc/load-image "ds.jpg"))
        gray (time (mc/filter-image (mf/grayscale) img))
        bw (time (mc/filter-image (mf/quantize 0) gray))
        blur (time (mc/filter-image (mf/blur) img))
        ]
    nil
    (time (ih/view bw))
    )
  )
