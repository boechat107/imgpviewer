(ns image-processing.image
  (:require [image-processing.pixel :as pix]))

;;; image structure
;[ [{:x :y | :a 0-255 :r 0-255 :g 0-255 :b 0-255 | :gray 0-255 | :bw 0-1}, ...], WIDTH]

(defrecord Image [pixels width])

(defn get-image-type
  "Returns the type of a image structure:
      :argb, :bw or :gray
      OR nil if it is not a image."
  [img]
  (let [pix (first (:pixels img))]
    (cond
      (:bw pix) :bw
      (:gray pix) :gray
      (and (:r pix) (:g pix) (:b pix)) :argb
      :else nil)))

(defn white-image [type width height]
  (let [white-pix (case type
                    :argb {:a 255 :r 255 :g 255 :b 255}
                    :gray {:gray 255}
                    :bw {:bw 1}
                    (throw (IllegalArgumentException. "Unknown image type")))]
    (Image. (repeat (* width height) white-pix) width)))


(defn get-pixel
  "Gets the pixel [x y] of a image structure."
  [img x y]
  (nth (:pixels img) (+ x (* y (:width img)))))


(defn get-height
  "Doc"
  [img]
  (/ (count (:pixels img)) (:width img)))


(defn get-pixels-of-line
  "Gets a lazy sequence with the pixels of a line."
  [img line]
  (nth (partition (:width img) (:pixels img)) 
       line))


(defn get-pixels-of-column
  "Gets a lazy sequence with the pixels of a column."
  [img column]
  (nth (partition (get-height img)
                  (apply interleave (partition (:width img)
                                               (:pixels img))))
       column))


(defn get-image-abs-coords
  "Gets a Image with the absolute coordinates setted according to the order of 
   the pixels."
  [pixels width]
  (Image. 
    (map #(assoc %1 :x (first %2) :y (second %2))
         pixels
         (for [y (range (/ (count pixels) width)), x (range width)] [x y]))
    width))



(defn get-subimage
;todo: Iterate over the lazy sequence and returns a lazy sequence.
; multimethod to handle with a LazySeq and with a Vector (for performance purpose).
  "Gets a subimage determined by a square with W width, H height and starting points X and
   Y."
  [img x y w h]
  ;; A vector structure is needed to use subvec.
  (let [pixels (if (vector? (:pixels img)) (:pixels img) (vec (:pixels img)))
        new-pixels (reduce #(apply conj %1 (subvec pixels %2 (+ %2 w)))
                           []
                           (range
                             (+ x (* y (:width img)))
                             (+ x (* (+ y h) (:width img)))
                             (:width img)))]
    (get-image-abs-coords new-pixels w)))


