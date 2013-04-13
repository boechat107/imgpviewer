(ns image-processing.processing
  (:require 
    [image-processing.core-new :as ipc]
    [incanter.core :as ic]
    )
  )

(defn argb-to-gray
  "Returns a new Image whose color space is the grayscale.
  Reference:
  http://en.wikipedia.org/wiki/Grayscale"
  [img]
  {:pre [(ipc/color-type? img)]}
  ;; Todo: Just two types of color spaces are assumed, argb and rgb.
  (let [[rm gm bm] (if (= :argb (:type img))
                     (rest (:channels img))
                     (:channels img))]
    (-> (ic/matrix-map #(+ (* 0.2126 %1) (* 0.7152 %2) (* 0.0722 %3))
                       rm gm bm)
        flatten
        (ipc/make-image (ipc/ncols img) :gray))))

(defn gray-to-argb
  "Repeats the only grayscale channel for each color channel and returns a new ARGB
  Image."
  [img]
  {:pre [(= :gray (:type img))]}
  (let [gray-ch (first (:channels img))]
    (ipc/make-image (conj (repeat 3 gray-ch)
                          (ic/matrix 255 (ipc/nrows img) (ipc/ncols img)))
                    :argb)))
