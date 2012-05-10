(ns image-processing.image)

(defprotocol Points
  "This abstracts the ways to access points."
  (get-point [this x y] "Returns the content of a specific point."))

(defrecord Image [points width]
           Points
           (get-point [img x y]
                      (when (< x width)
                        (get points (+ x (* y width))))))


(defn image?
  "Returns true if ARG is a Image, false otherwise."
  #^{:arglists [arg]}
  [arg]
  (= (type arg) (type (Image. 0 0))))

