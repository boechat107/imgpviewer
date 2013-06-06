(ns image-processing.core-new
  (:require 
    [incanter.core :as ic]))

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

(defn doubles?
  [x]
  (= (type x) (Class/forName "[D")))

(defn ints?
  [x]
  (= (type x) (Class/forName "[I")))

(defn mat?
  [m]
  (and (sequential? m)
       (every? doubles? m)))

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
  ^long [img]
  ((:type img) color-dimensions))

(defn new-channel-matrix 
  "Returns a matrix used to represent a color channel data."
  [nrows ncols] 
  (int-array (* nrows ncols)))

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

(defn copy-image
  "Returns a copy of a given image."
  [img]
  (->> (mapv #(aclone ^ints %) (:mat img)) 
       (assoc img :mat)))

(defmacro mult-aget
  "Returns the value of an element of multiple dimensional arrays. Uses type hints to 
  improve the performance of aget.
  Reference:
  http://clj-me.cgrand.net/2009/10/15/multidim-arrays/"
  ([hint array idx]
   `(aget ~(vary-meta array assoc :tag hint) ~idx))
  ([hint array idx & idxs]
   `(let [a# (aget ~(vary-meta array assoc :tag 'objects) ~idx)]
      (mult-aget ~hint a# ~@idxs))))

(defn get-pixel
  "Returns the value of the pixel [x, y]. If no channel is specified, a vector is
  returned; otherwise, a scalar is returned.
  For a better performance, use the macro mult-aget."
  (^long [img x y ch]
   (ut/mult-aget ints ((:mat img) ch) (+ (* y (ncols img)) x)))
  (^long [img idx ch]
   (ut/mult-aget ints ((:mat img) ch) idx)))

(defmacro get-ch-pixel
  "Returns the value of a pixel of a specific channel. ch-a should be a array of
  ints."
  ([ch-a idx]
   `(ut/mult-aget ~'ints ~ch-a ~idx))
  ([ch-a x y nc]
   `(ut/mult-aget ~'ints ~ch-a (+ ~x (* ~y ~nc)))))

(defmacro mult-aset
  "Sets the value of an element of a multiple dimensional array. Uses type hints to 
  improve the performance of aset. (Only for double and int arrays for now)
  Reference:
  http://clj-me.cgrand.net/2009/10/15/multidim-arrays/"
  [hint array & idxsv]
  (let [hints '{doubles double ints int bytes byte longs long}
        [v idx & sxdi] (reverse idxsv)
        idxs (reverse sxdi)
        v (if-let [h (hints hint)] (list h v) v)
        nested-array (if (seq idxs)
                       `(mult-aget ~'objects ~array ~@idxs)
                       array)
        a-sym (with-meta (gensym "a") {:tag hint})]
    `(let [~a-sym ~nested-array]
       (aset ~a-sym ~idx ~v))))

(defn set-pixel!
  "Sets the value of the [x, y] pixel. For a better performance, use the macro
  mult-aset."
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
  ([f x-min x-max y-min y-max]
   (doseq [y (range y-min y-max), x (range x-min x-max)]
     (f x y)))
  ([^long nr ^long nc f]
   (dotimes [x nc]
     (dotimes [y nr]
       (f (+ x (* y nc)))))))

(defmacro for-idx
  "Iterates over all pixels of img, binding the pixel's index to idx.
  Ex.:
  (for-idx [idx img]
    body)"
  [[idx img] & body]
  `(let [nr# (nrows ~img)
         nc# (ncols ~img)]
     (dotimes [x# nc#]
       (dotimes [y# nr#]
         (let [~idx (+ x# (* y# nc#))]
           ~@body)))))

(defmacro for-xy
  "Iterates over all pixels of an image, binding the pixels' index to [x, y]."
  [[x y img] & body]
  `(let [nr# (nrows ~img)
         nc# (ncols ~img)]
     (dotimes [~x nc#]
       (dotimes [~y nr#]
         ~@body))))

(defmacro for-chs
  "Binds the channels of images to a local variable.
  Ex.:
  (for-chs [img-ch img, res-ch res]
    body)"
  [chs-imgs & body]
  `(let [])
  )
