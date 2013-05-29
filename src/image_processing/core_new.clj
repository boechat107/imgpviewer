(ns image-processing.core-new
  (:require 
    [image-processing.utils :as ut]
    [incanter.core :as ic]
    ))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defrecord Image [mat type nrows ncols])

(def color-dimensions
  {:rgb 3
   :argb 4
   :gray 1})

(defn image?
  [obj]
  (instance? Image obj))

(defn valid-type?
  [type]
  (some #(= type %) [:argb :rgb :gray]))

(defn mat?
  [m]
  (= (type m) (Class/forName "[[[I")))

(defn color-type?
  [img]
  (some #(= % (:type img)) [:argb :rgb]))

(defn gray-type?
  [img]
  (= :gray (:type img)))

(defn argb-type?
  [img]
  (= :argb (:type img)))

(defn nrows
  "Returns the number of rows of an Image."
  [img]
  (:nrows img))

(defn ncols
  "Returns the number of rows of an Image."
  [img]
  (:ncols img))

(defn dimension 
  "Returns the number of the dimensions of the image's color space."
  [img]
  ((:type img) color-dimensions))

(defn new-channel-matrix 
  "Returns a matrix used to represent a color channel data."
  [nrows ncols dim] 
  (make-array Integer/TYPE nrows ncols dim))

(defn make-image
  "Returns an instance of Image for a given image data, its number of columns of
  pixels and the color space of the image. 
  The image data is stored as different channels, each one as a clojure.matrix, and
  the value of each pixel a double value."
  ([data-chs type]
   {:pre [(valid-type? type) (mat? data-chs)]}
   (Image. data-chs
           type
           (alength #^objects data-chs) 
           (alength #^objects (aget #^objects data-chs 0)))))

(defn new-image
  "Returns an empty image with the given dimension and color type."
  [nrows ncols type]
  {:pre [(contains? color-dimensions type)]}
  (-> (new-channel-matrix nrows ncols (type color-dimensions))
      (make-image type)))

;(defn copy-image
;  "Returns a copy of a given image."
;  [img]
;  (-> (map #(ut/mult-aclone %) (:mat img)) 
;      (make-image (:type img))))

(defn get-pixel
  "Returns the value of the pixel [x, y]. If no channel is specified, a vector is
  returned; otherwise, a scalar is returned."
  ([img x y]
   (mapv #(ut/mult-aget ints (:mat img) y x %) (range (dimension img))))
  ([img x y ch]
   (ut/mult-aget ints (:mat img) y x ch)))

(defn get-neighbour
  "Returns the value of one of the pixels of a squared area around [x,y].
  [x, y] has pos=4.
  [0 1 2
  3 4 5
  6 7 8]
  "
  [img x y ch pos]
  (let [real-xy (fn [c m]
                  (let [c (long c), m (long m)]
                    (min (dec m) (max 0 c))))
        x (long x)
        y (long y)
        pos (long pos)]
    (get-pixel img
      (real-xy (+ (dec x) (rem pos 3)) (ncols img)) 
      (real-xy (+ (dec y) (quot pos 3)) (nrows img))
      ch)))

(defn set-pixel!
  "Sets the value of the [x, y] pixel."
  ([img x y vals]
   (dorun 
     (map-indexed #(ut/mult-aset ints (:mat img) y x %1 %2)
                  vals)))
  ([img x y ch val]
   (ut/mult-aset ints (:mat img) y x ch val)))

(defn grid-apply
  "Returns a lazy sequence resulting from the application of the function f to each 
  value of the grid built with the rectangle x-min, x-max, y-min, y-max."
  ;; todo: turn it into a side effect function.
  ([f x-min x-max y-min y-max]
   (doseq [y (range y-min y-max), x (range x-min x-max)]
     (f x y)))
  ([f img]
   (dotimes [x (ncols img)]
     (dotimes [y (nrows img)]
       (f x y)))))

(defn pgrid-apply
  "Like grid-apply, but the rows are processed in parallel."
  [f x-min x-max y-min y-max]
  (doall
    (pmap (fn [y]
            (doall
              (map #(f % y) (range x-min x-max))))
          (range y-min y-max))))
