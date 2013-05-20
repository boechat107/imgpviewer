(ns image-processing.core-new
  (:require 
    [image-processing.utils :as ut]
    [incanter.core :as ic]
    ))

(defrecord Image [chs type nrows ncols])

(defn image?
  [obj]
  (instance? Image obj))

(defn channel?
  [obj]
  ;; todo: better definition.
  (= (type obj) (type (make-array Integer/TYPE 1 1))))

(defn valid-type?
  [type]
  (some #(= type %) [:argb :rgb :gray]))



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

(defn new-channel-matrix 
  "Returns a matrix used to represent a color channel data."
  ;; todo: pull request to mikera to fill a matrix.
  ;; problem with x y indexing
  ([nrows ncols] 
   (make-array Integer/TYPE nrows ncols)))

(defn make-image
  "Returns an instance of Image for a given image data, its number of columns of
  pixels and the color space of the image. 
  The image data is stored as different channels, each one as a clojure.matrix, and
  the value of each pixel a double value."
  ([data-chs type]
   {:pre [(valid-type? type) (every? channel? data-chs)]}
   (let [ch (first data-chs)]
     (Image. (if (vector? data-chs) data-chs (vec data-chs))
             type (alength ch) (alength (aget ch 0))))))

(defn get-pixel
  "Returns the value of the pixel [x, y]. If no channel is specified, a vector is
  returned; otherwise, a scalar is returned."
  ([img x y]
  (->> (:chs img)
       (mapv #(aget ints % y x))))
  ([img x y ch]
   (ut/mult-aget ints ((:chs img) ch) y x)))

(defn set-pixel!
  "Sets the value of the [x, y] pixel."
  ([img x y vals]
   (dorun 
     (map-indexed #(ut/mult-aset ints ((:chs img) %1) y x %2)
                  vals)))
  ([img x y ch val]
   (ut/mult-aset ints ((:chs img) ch) y x val)))

;(defn img-map
;  "Applies a function f to each pixel of an image, over each channel of the pixel.
;  The function f should accept a scalar representing the value of a color channel of
;  the pixel.
;  Returns a new image with the same color space."
;  ([f img]
;   (-> (mapv #(mx/emap f %) (:chs img))
;       (make-image (:type img))))
;  ([f img1 img2]
;   {:pre [(= (:type img1) (:type img2))]}
;   (-> (mapv #(mx/emap f %1 %2) (:chs img1) (:chs img2))
;       (make-image (:type img1))))
;  ([f img1 img2 & imgs]
;   (->> (conj imgs img2 img1) 
;        (map :chs)
;        (apply mapv (fn [& ms] (apply mx/emap f ms)))
;        (#(make-image % (:type img1))))))
;  
;(defn chs-map
;  "Like img-map, but f should accept a vector of scalars, each one representing the pixel
;  value of a color channel. 
;  Returns a new image with just one color channel."
;  [f img]
;  (let [new-ch (mx/new-matrix (nrows img) (ncols img))
;        n-chs (count (:chs img))
;        chs-vals (double-array n-chs)]
;    (dotimes [r (nrows img)]
;      (dotimes [c (ncols img)]
;        (->> (map #(mx/mget % r c) (:chs img))
;             vec
;             (apply f)
;             (mx/mset! new-ch r c))))
;    (make-image [new-ch] :gray)))

(defn grid-apply
  "Returns a lazy sequence resulting from the application of the function f to each 
  value of the grid built with the rectangle x-min, x-max, y-min, y-max."
  [f x-min x-max y-min y-max]
  (for [y (range y-min y-max), x (range x-min x-max)]
    (f x y)))

(defn pgrid-apply
  "Like grid-apply, but the rows are processed in parallel."
  [f x-min x-max y-min y-max]
  (doall
    (pmap (fn [y]
            (doall
              (map #(f % y) (range x-min x-max))))
          (range y-min y-max))))
