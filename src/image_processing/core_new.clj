(ns image-processing.core-new
  (:require 
    [incanter.core :as ic]
    [clojure.core.matrix :as mx]
    [mikera.vectorz.matrix :as mz]
    ))

(mx/set-current-implementation :vectorz)

(defrecord Image [chs type])

(defn image?
  [obj]
  (instance? Image obj))

(defn valid-type?
  [type]
  (some #(= type %) [:argb :rgb :gray]))

(defn mat?
  [obj]
  (mx/matrix? obj))

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
  (mx/row-count (first (:chs img))))

(defn ncols
  "Returns the number of rows of an Image."
  [^Image img]
  (mx/column-count (first (:chs img))))

(defn make-image
  "Returns an instance of Image for a given image data, its number of columns of
  pixels and the color space of the image. 
  The image data is stored as different channels, each one as a clojure.matrix, and
  the value of each pixel a double value."
  ([data-chs type]
   {:pre [(valid-type? type) (every? mz/matrix? data-chs)]}
   (Image. data-chs type)))

(defn get-pixel
  "Returns the value of the representation of pixel [x, y], where x increases 
  for columns."
  [img x y]
  (if (color-type? img)
    (-> (mx/slice (:mat img) y)
        (mx/mget x))
    (mx/mget (:mat img) x y)))

(defn get-seq-pixels 
  "Returns a sequence that contains the value of each pixel, one row after the
  other."
  [img]
  (let [pixs (mx/eseq (:mat img))]
    (condp = (:type img)
      :gray pixs
      :rgb (partition 3 pixs)
      :argb (partition 4 pixs))))

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
