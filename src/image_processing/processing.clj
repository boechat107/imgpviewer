(ns image-processing.processing
  (:require 
    [image-processing.core :as ipc]
    [incanter.core :as ic]
    )
  )

(defn color-to-gray
  "Returns a new Image whose color space is the grayscale.
  Reference:
  http://en.wikipedia.org/wiki/Grayscale"
  [img]
  {:pre [(ipc/color-type? img)]}
  ;; Todo: Just two types of color spaces are assumed, argb and rgb.
  (let [[rm gm bm] (if (= :argb (:type img))
                     (rest (:channels img))
                     (:channels img))])
  (ic/matrix-map #(+ (* 0.2126 %1) (* 0.7152 %2) (* 0.0722 %3))
                 rm gm bm))
