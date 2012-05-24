(ns image-processing.image)

(defprotocol Points
  "This abstracts the ways to access points."
  (get-subset [this x y w h] "Returns the subset defined by a rectangular region.")
  (get-point [this x y] "Returns the content of a specific point."))

(defrecord Image [points width]
  Points
  (get-subset [img x y w h]
              (reduce #(apply conj %1 (subvec points %2 (+ %2 w)))
                      []
                      (range
                        (+ x (* y width))
                        (+ x (* (+ y h) width))
                        width)))
  (get-point [img x y]
             (when (< x width)
               (get points (+ x (* y width))))))

(defn get-img-height
  "Doc"
  #^{:arglists [img]}
  [img]
  (/ (count (:points img)) (:width img)))


(defn get-subimage
  "Doc"
  #^{:arglists [img x y w h]}
   [img x y w h]
   (Image.
     (reduce #(apply conj %1 (subvec (:points img) %2 (+ %2 w)))
             []
             (range
               (+ x (* y (:width img)))
               (+ x (* (+ y h) (:width img)))
               (:width img)))
     w))


(defn image?
  "Returns true if ARG is a Image, false otherwise."
  #^{:arglists [arg]}
  [arg]
  (instance? image_processing.image.Image arg))

