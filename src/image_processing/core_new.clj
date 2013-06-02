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
  (and (sequential? m)
       (every? #(= (type %) (Class/forName "[I")) m)))

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
  ^long [img]
  (:nrows img))

(defn ncols
  "Returns the number of rows of an Image."
  ^long [img]
  (:ncols img))

(defn dimension 
  "Returns the number of the dimensions of the image's color space."
  [img]
  ((:type img) color-dimensions))

(defn new-channel-matrix 
  "Returns a matrix used to represent a color channel data."
  [nrows ncols] 
  (make-array Integer/TYPE (* nrows ncols)))

(defn make-image
  "Returns an instance of Image for a given image data, its number of columns of
  pixels and the color space of the image. 
  The image data is stored as different channels, each one as a clojure.matrix, and
  the value of each pixel a double value."
  ([data-chs nrows ncols type]
   {:pre [(valid-type? type) (mat? data-chs)
          (every? #(== (alength ^ints %) (* nrows ncols)) data-chs)]}
   (Image. (if (vector? data-chs) data-chs (vec data-chs))
           type nrows ncols)))

(defn new-image
  "Returns an empty image with the given dimension and color type."
  [nrows ncols type]
  {:pre [(contains? color-dimensions type)]}
  (-> (type color-dimensions)
      (repeatedly #(new-channel-matrix nrows ncols))
      (make-image nrows ncols type)))

;(defn copy-image
;  "Returns a copy of a given image."
;  [img]
;  (-> (map #(ut/mult-aclone %) (:mat img)) 
;      (make-image (:type img))))

(defn xy-to-pos 
  "Converts a index [x, y] to a index for a continuous array."
  [x y ncols]
  (-> (* y ncols) (+ x)))

(defn get-pixel
  "Returns the value of the pixel [x, y]. If no channel is specified, a vector is
  returned; otherwise, a scalar is returned."
  (^long [img x y ch]
   (ut/mult-aget ints ((:mat img) ch) (+ (* y (ncols img)) x)))
  (^long [img idx ch]
   (ut/mult-aget ints ((:mat img) ch) idx)))

(defn get-neighbour
  "Returns the value of one of the pixels of a squared area around [x,y].
  [x, y] has pos=4.
  [0 1 2
  3 4 5
  6 7 8]
  "
  [img x y ch pos]
  (let [real-xy (fn ^long [^long c ^long m]
                  (min (dec m) (max 0 c)))
        x (long x)
        y (long y)
        pos (long pos)]
    (get-pixel img
               (real-xy (cond
                          (or (== pos 0) (== pos 3) (== pos 6)) 0
                          (or (== pos 1) (== pos 4) (== pos 7)) 1
                          :else 2)
                        (ncols img))
               (real-xy (cond
                          (> 3 pos) 0
                          (> 6 pos) 1
                          :else 2)
                        (nrows img))
               ch)))

(defn set-pixel!
  "Sets the value of the [x, y] pixel."
  ([img x y ch val]
   (ut/mult-aset ints ((:mat img) ch)
                 (+ x (* y (ncols img))) 
                 val))
  ([img idx ch val]
   (ut/mult-aset ints ((:mat img) ch)
                 idx 
                 val)))

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
