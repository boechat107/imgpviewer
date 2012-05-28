(ns image-processing.image)

;;; image structure
;[ [{:x :y | :a 0-255 :r 0-255 :g 0-255 :b 0-255 | :gray 0-255 | :bw 0-1}, ...], WIDTH]

(defrecord Image [pixels width])

(defn get-image-type
  "Returns the type of a image structure:
      :argb, :bw or :gray
      OR nil if it is not a image."
  #^{:arglists [img]}
  [img]
  (let [pix (first (:pixels img))]
    (cond
      (:bw pix) :bw
      (:gray pix) :gray
      (and (:r pix) (:g pix) (:b pix)) :argb
      :else nil)))


(defn get-pixel
  "Gets the pixel [x y] of a image structure."
  #^{:arglists [img x y]}
  [img x y]
  (get (:pixels img) (+ x (* y (:width img)))))


(defn get-height
  "Doc"
  #^{:arglists [img]}
  [img]
  (/ (count (:pixels img)) (:width img)))


;todo: Iterate over the lazy sequence and returns a lazy sequence.
; multimethod to handle with a LazySeq and with a Vector (for performance purpose).
(defn get-subimage
  "Doc"
  [img x y w h]
  (Image.
    ;; A vector structure is needed to use subvec.
    (let [pixels (if (vector? (:pixels img)) (:pixels img) (vec (:pixels img)))
          new-pixels (reduce #(apply conj %1 (subvec pixels %2 (+ %2 w)))
                             []
                             (range
                               (+ x (* y (:width img)))
                               (+ x (* (+ y h) (:width img)))
                               (:width img)))]
      (map #(assoc %1 :x (first %2) :y (second %2))
           new-pixels
           (for [y (range h), x (range w)] [x y]))) 
    w))




