(ns image-processing.core-new
  (:require 
    [incanter.core :as ic]
    ))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defrecord Image [mat type ^long nrows ^long ncols])

(defn image?
  [obj]
  (instance? Image obj))

(def color-dimensions
  {:rgb 3
   :argb 4
   :gray 1})

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
  ^long [^Image img]
  (:nrows img))

(defn ncols
  "Returns the number of rows of an Image."
  ^long [^Image img]
  (:ncols img))

(defn dimension 
  "Returns the number of the dimensions of the image's color space."
  [img]
  ((:type img) color-dimensions))

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
  ([data-mat nrows ncols type]
   {:pre [(valid-type? type)]}
   (Image. (if (vector? data-mat) data-mat (vec data-mat))
           type nrows ncols)))

(defn get-xy
  "Returns the value of the representation of pixel [x, y], where x increases 
  for columns."
  (^long [img ^long x ^long y]
   ((:mat img) (+ x (* y (ncols img)))))
  (^long [img ^long x ^long y ch]
   {:pre [(> (dimension img) 1)]}
   (((:mat img) (+ x (* y (ncols img)))) ch)))

(defn get-pix-1d
  ([img idx]
   ((:mat img) idx))
  ([img idx ch]
   {:pre [(> (dimension img) 1)]}
   (((:mat img) idx) ch)))

(defn get-neighbour
  "Returns the value of one of the pixels of a squared area around [x,y].
  [x, y] has pos=4.
  [0 1 2
  3 4 5
  6 7 8]
  "
  ([img idx pos])
  (^long [img x y pos]
  (let [real-xy (fn ^long [^long c ^long m]
                  (min (dec m) (max 0 c)))]
    (get-xy img
            (real-xy (cond
                       (or (== pos 0) (== pos 3) (== pos 6)) 0
                       (or (== pos 1) (== pos 4) (== pos 7)) 1
                       :else 2)
                     (ncols img))
            (real-xy (cond
                       (> 3 pos) 0
                       (> 6 pos) 1
                       :else 2)
                     (nrows img))))))

(defn mat-map
  "Applies a function f to each element of the matrix mat, returning a new mat
  structure (vector of vectors) with the new values."
  ([f mat]
   (mapv f mat)))

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
