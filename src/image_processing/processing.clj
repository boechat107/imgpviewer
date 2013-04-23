(ns image-processing.processing
  (:require 
    [image-processing.core-new :as ipc]
    [image-processing.utils :as ut]
    [incanter.core :as ic]
    )
  )

(defn rgb-to-gray
  "Returns a new Image whose color space is the grayscale.
  Reference:
  http://en.wikipedia.org/wiki/Grayscale"
  [img]
  {:pre [(= :rgb (:type img))]}
  (->> (:mat img)
       ;; todo: only rgb is covered.
       (ipc/mat-map #(let [[r g b] %]
                       (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))
       (#(ipc/make-image % :gray))))

(defn gray-to-rgb
  "Repeats the only grayscale channel for each color channel and returns a new RGB
  Image."
  [img]
  {:pre [(= :gray (:type img))]}
  (-> (ipc/mat-map #(vector % % %) (:mat img))
      (ipc/make-image :rgb)))

(defn rgb-to-argb 
  "Adds the transparency channel to a rgb Image."
  [img]
  {:pre [(= :rgb (:type img))]}
  (-> (ipc/mat-map #(concat [255] %) (:mat img))
      (ipc/make-image :argb)))

(defn gray-to-argb 
  "Converts the color space from grayscale to ARGB."
  [img]
  {:pre [(= :gray (:type img))]}
  (-> (ipc/mat-map #(vector 255 % % %) (:mat img))
      (ipc/make-image :argb)))

(defn grid-apply
  "Returns a sequence resulting from the application of the function f to each 
  value of the grid built with the rectangle x-min, x-max, y-min, y-max."
  [f x-min x-max y-min y-max]
  (for [y (range y-min y-max), x (range x-min x-max)]
    (f x y)))

(defn- if-map 
  "If the argument is a collection, applies f to every element, returning a vector
  (mapv is used); otherwise, f is directly applied to the argument.
  The initial purpose of this function is apply operations to images of different
  color spaces, like grayscale (pixels value are just numbers) and rgb (pixels values
  are a vector of numbers)."
  [f a]
  (if (coll? a) (mapv f a) (f a)))

(defn convolve
  [img mask]
  ;; todo: speed up with discrete Fourier transform.
  ;; variable mask size.
  (let [nr (ipc/nrows img)
        nc (ipc/ncols img)
        sample (ipc/get-xy img 0 0)
        ;; Dimensionality of the color space.
        nv (when (coll? sample) (count sample))
        real-xy (fn [c m] 
                  ;; Returns c if it is between the boundaries of the image. 
                  (min (dec m) (max 0 c)))
        kernel (fn [x y] 
                 ;; Apply the mask on a pixel given by [x,y] and its neighbor pixels.
                 (->> (for [ky (range (dec y) (+ 2 y)),
                            kx (range (dec x) (+ 2 x))]
                        (ipc/get-xy img (real-xy kx nc) (real-xy ky nr)))
                      ;; Multiplication of each pixel of the mask.
                      (map #(ut/mult-vec %1 %2) mask)
                      (reduce #(if nv (map + %1 %2) (+ %1 %2)) 
                              (if nv (repeat nv 0) 0))
                      (if-map #(min 255 %))
                      (if-map #(max 0 %))))]
    (->> (grid-apply kernel 0 nc 0 nr)
         (partition nc)
         (mapv vec)
         (#(ipc/make-image % (:type img))))))

(defn erode
  "Erodes a Image, a basic operation in the area of the mathematical morphology.
   http://homepages.inf.ed.ac.uk/rbf/HIPR2/erode.htm
   The corner and edge values of the mask can be specified. The default values are 0.2."
   ([img] (erode img 0.2 0.2))
   ([img corner edge]
    {:pre [(ipc/gray-type? img)]}
    (let [mask [corner  edge    corner
                edge    1.0     edge
                corner  edge    corner]]
      (convolve img mask))))

(defn binarize
  "Returns a new Image where each pixel value is set to 0 or 255. If the original
  pixel value is below the threshold, the value is set to 0; otherwise, the value is
  set to 255."
  [img th]
  (letfn [(threshold [n] (if (> n th) 255 0))]
    (-> (ipc/mat-map #(if-map threshold %) (:mat img))
        (ipc/make-image (:type img)))))
