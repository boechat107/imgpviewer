(ns image-processing.core-new
  (:require 
    [incanter.core :as ic]
    ))

(defrecord Image [mat type])

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
  (count (:mat img)))

(defn ncols
  "Returns the number of rows of an Image."
  [^Image img]
  (count (first (:mat img))))

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
   (Image.  data-mat type))
  ([data-array ncols type]
   {:pre [(valid-type? type) (coll? data)]}
   (letfn [(constructor [m] (Image. m type))]
     (->> data-array 
       (partition ncols)
       (map vec)
       vec
       constructor))))

(defn get-xy
  "Returns the value of the representation of pixel [x, y], where x increases 
  for columns."
  [img x y]
  (get-in (:mat img) [y x]))

(defn mat-map
  "Applies a function f to each element of the matrix mat, returning a new mat
  structure with the new values."
  ([f mat]
   (map (fn [r]
          
          ))
   )
  )
