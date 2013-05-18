(ns image-processing.core-new
  (:require 
    [incanter.core :as ic]
    ))

(defrecord Image [mat type nrows ncols])

(defn image?
  [obj]
  (instance? Image obj))

(defn valid-type?
  [type]
  (some #(= type %) [:argb :rgb :gray]))

(defn mat?
  [obj]
  (and (vector? obj) (every? vector? obj)))

(defn color-type?
  [img]
  (some #(= % (:type img)) [:argb :rgb]))

(defn gray-type?
  [img]
  (= :gray (:type img)))

(defn argb-type?
  [img]
  (= :argb (:type img)))

;;; 
;;; Functions that depends of the library used to represent the image information.
;;; 

(defn nrows
  "Returns the number of rows of an Image."
  [^Image img]
  (:nrows img))

(defn ncols
  "Returns the number of rows of an Image."
  [^Image img]
  (:ncols img))

(defn make-image
  "Returns an instance of Image for a given image data, its number of columns of
  pixels and the color space of the image. 
  data-mat must be something like this:
  [[    ]
   [    ]
   ...
   [    ]]
  data-array must be just a list of vectors (for RGB images) or of numbers, sorted by
  their position in the image.
  0 +----------------+ w - 1
    |                |
    |                |
    +----------------+ h*w - 1"
  ([data-mat type]
   {:pre [(valid-type? type) (vector? data-mat) (every? vector? data-mat)]}
   (Image. data-mat type (count data-mat) (count (first data-mat)))))

(defn get-xy
  "Returns the value of the representation of pixel [x, y], where x increases 
  for columns."
  [img x y]
  (get-in (:mat img) [y x]))

(defn mat-map
  "Applies a function f to each element of the matrix mat, returning a new mat
  structure (vector of vectors) with the new values."
  ([f mat]
   (mapv #(mapv f %) mat))
  ([f m & ms]
   (apply mapv (fn [& a] (apply mapv f a)) m ms)))

(defn mat-pmap
  "Like mat-map, except f is applied in parallel."
  ([f mat]
   ;; todo
   )
  )

(defn mat-map-indexed 
  "Like mat-map, but the function must receive the row and column indexes as the
  first two arguments."
  ([f mat]
   (mapv (fn [r y]
           (mapv #(f %2 y %1) r (range (count r))))
         mat (range (count mat)))))
