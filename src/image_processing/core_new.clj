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
  (->> (:chs img)
       (mapv #(mx/mget % x y))))

(defn img-map
  "Applies a function f to each pixel of an image. The function f should accept a 
  vector with the value of each color channel of the pixel.
  Returns a new image with the same color space."
  ([f img]
   (-> (mapv #(mx/emap f %) (:chs img))
       (make-image (:type img))))
  ([f img1 img2]
   {:pre [(= (:type img1) (:type img2))]}
   (-> (mapv #(mx/emap f %1 %2) (:chs img1) (:chs img2))
       (make-image (:type img1))))
  ([f img1 img2 & imgs]
   (->> (conj imgs img2 img1) 
        (map :chs)
        (apply mapv (fn [& ms] (apply mx/emap f ms)))
        (#(make-image % (:type img1))))))
  
(defn chs-map
  "Like img-map, but f should accept a vector of scalars, each one representing the pixel
  value of a color channel. 
  Returns a new grayscale image with just one color channel."
  [f img]
   (-> (apply mx/emap f (:chs img))
       (make-image :gray)))

; (defn mat-pmap
;   "Like mat-map, except f is applied in parallel."
;   ([f mat]
;    ;; todo
;    )
;   )
; 
; (defn mat-map-indexed 
;   "Like mat-map, but the function must receive the row and column indexes as the
;   first two arguments."
;   ([f mat]
;    (mapv (fn [r y]
;            (mapv #(f %2 y %1) r (range (count r))))
;          mat (range (count mat)))))
