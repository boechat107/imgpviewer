(ns image-processing.image
  (:require [image-processing.pixel :as pix])
  (:import [java.lang Comparable]))

;;; image structure
;[ [{:x :y | :a 0-255 :r 0-255 :g 0-255 :b 0-255 | :gray 0-255 | :bw 0-1}, ...], WIDTH]

(defrecord Image [pixels width])

(defn is-img? [Img]
  (= (type Img) image_processing.image.Image))

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

(defmacro switch-image-type [img expr-argb expr-gray expr-bw]
  `(case (get-image-type ~img)
     :argb ~expr-argb
     :gray ~expr-gray
     :bw ~expr-bw
     (throw (IllegalArgumentException. "Image is not :argb, nor :gray, nor :bw. What should i do?"))))

(defn white-image [type width height]
  (let [white-pix (pix/WHITE type)]
    (Image. (vec  (repeat (* width height) white-pix)) width)))

(defn blank-image
  "A blank image must be an :a :r :g :b for handling transparency"
  [width height]
  (Image. (vec (repeat (* width height) {:a 0 :r 0 :g 0 :b 0})) width))

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
  ([img]
     (partition (:width img) (:pixels img)))
  ([img line]
     (nth (get-pixels-of-line img) line)))


(defn get-pixels-of-column
  "Gets a lazy sequence with the pixels of a column."
  ([img]
     (partition (get-height img)
                (apply interleave (partition (:width img)
                                             (:pixels img)))))
  ([img column]
     (nth (get-pixels-of-column img) column)))


(defn get-image-abs-coords
  "Gets a Image with the absolute coordinates setted according to the order of 
   the pixels."
  ([img]
     (let [pixels (:pixels img)
           width (:width img)]
       (get-image-abs-coords pixels width)))
  ([pixels width]
     (Image. 
      (mapv #(assoc %1 :x (first %2) :y (second %2))
           pixels
           (for [y (range (/ (count pixels) width)), x (range width)] [x y]))
      width)))


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

(defn increase-size
  "Changes the size of the image by increasing in the four supplied direction"
  ([img [north east south west :as coords]]
     (let [type (get-image-type img)]
       (increase-size img coords (pix/WHITE type))))
  ([img [north east south west :as coords] background-pix]
     {:pre [(= (pix/pix-type background-pix)
               (get-image-type img))
            (every? #(<= 0 %) coords)]}
     (let [width (:width img)
           height (get-height img)
           new-width (+ west east width)
           new-height (+ north south height)]
       (Image.
        (vec (concat
              (repeat (* north new-width) background-pix)
              (mapcat concat
                      (repeat (repeat west background-pix))
                      (get-pixels-of-line img)
                      (repeat (repeat east background-pix)))
              (repeat (* south new-width) background-pix)))
        new-width
        ))))