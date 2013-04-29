(ns image-processing.rgb-color
  "Colors are expected as vector [r g b] with values in the interval [0, 255]."
  (:require 
    [incanter.core :as ic]
    )
  )

(def color
  {:black [0 0 0]
   :white [255 255 255]})

(defn abs-distance
  "Returns the absolute distance between rgb colors. Also known as Manhattan distance."
  [c1 c2]
  (->> (map - c1 c2)
       (map ic/abs)
       ic/sum))

(defn euclidian-distance
  "Returns the Euclidian distance between rgb colors.
  sqrt((r1 - r2)^2 + (g1 - g2)^2 + (b1 - b2)^2)"
  [c1 c2]
  (->> (map - c1 c2)
       (map #(ic/pow % 2))
       ic/sum
       ic/sqrt))
