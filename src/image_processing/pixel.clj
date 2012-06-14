(ns image-processing.pixel)

(defn grayscale-value [pixel]
  (/ (apply min ((juxt :r :g :b) pixel))
     255))