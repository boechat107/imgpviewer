(ns image-processing.processing
  (:require 
    [image-processing.core-new :as ipc]
    [image-processing.utils :as ut]
    [image-processing.rgb-color :as rgb]
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
  (mapv (fn [y]
          (mapv #(f % y) (range x-min x-max)))
        (range y-min y-max)))

(defn pgrid-apply
  "Like grid-apply, but the function is applied in parallel."
  [f x-min x-max y-min y-max]
  (vec
    (pmap (fn [y]
            (mapv #(f % y) (range x-min x-max)))
          (range y-min y-max))))

(defn- if-map 
  "If the argument is a collection, applies f to every element, returning a vector
  (mapv is used); otherwise, f is directly applied to the argument.
  The initial purpose of this function is apply operations to images of different
  color spaces, like grayscale (pixels value are just numbers) and rgb (pixels values
  are a vector of numbers)."
  [f a]
  (if (coll? a) (mapv f a) (f a)))

(defn get-neighbour-pixels
  "Returns the 9 pixels of a squared area around the [x, y] pixel. If a neighbor
  pixel is outside the boundaries of the image, the nearest pixel is returned.
  [0 1 2
  3 4 5
  6 7 8]"
  [img x y]
  (let [real-xy (fn [c m] 
                  ;; Returns c if it is between the boundaries of the image. 
                  (min (dec m) (max 0 c)))]
    (vec (for [ky [(dec y) y (inc y)],
          kx [(dec x) x (inc x)]]
      (ipc/get-xy img 
                  (real-xy kx (ipc/ncols img))
                  (real-xy ky (ipc/nrows img)))))))

(defn apply-kernel 
  "Just applies a kernel mask to a [x, y] pixel and its neighbors."
  [img x y mask]
  (let [sample (ipc/get-xy img 0 0)
        ;; Dimensionality of the color space.
        nv (when (coll? sample) (count sample))]
    (->> (get-neighbour-pixels img x y)
         ;; Multiplication of each pixel of the mask.
         (map #(ut/mult-vec %1 %2) mask)
         (reduce #(if nv (map + %1 %2) (+ %1 %2)) 
                 (if nv (repeat nv 0) 0))
         (if-map #(min 255 %))
         (if-map #(max 0 %)))))

(defn apply-kernel-one
  "Applies a convolution kernel for one channel images."
  [img x y mask]
  (->> (get-neighbour-pixels img x y)
       (map * mask)
       (reduce +)
       (min 255)
       (max 0)))

(defn convolve
  [img mask]
  ;; todo: speed up with discrete Fourier transform.
  ;; variable mask size.
  (let [nr (ipc/nrows img)
        nc (ipc/ncols img)]
    (->> (grid-apply #(apply-kernel-one img %1 %2 mask) 0 nc 0 nr)
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

(defn smoothing
  "Returns a new Image resulting of the application of a edge preserving smoothing on
  the given Image.
  -----------
  Reference: 
  Nikolaou, N., & Papamarkos, N. (2009). Color reduction for complex document images.
  International Journal of Imaging Systems and Technology, 19(1), 14–26.
  doi:10.1002/ima.20174"
  [img]
  {:pre [(ipc/color-type? img)]}
  (letfn [;; Calculates the coefficient value for a pixel.
          (coef [c1 c2] 
               (-> (rgb/abs-distance c1 c2)
                   (/ (* 3 255))
                   (#(- 1 %))
                   (ic/pow 10)))
          ;; Calculates the new value of the pixel [x, y] by applying a convolution.
          (pix-val [x y]
            (->> (get-neighbour-pixels img x y)
                 (map #(coef (ipc/get-xy img x y) %))
                 ((fn [cs] (map #(/ % (ic/sum cs)) cs)))
                 (apply-kernel img x y)))]
    (->> (reduce (fn [m y]
                   (->> (reduce #(conj! %1 (pix-val %2 y))
                                (transient [])
                                (range (ipc/ncols img)))
                        persistent!
                        (conj! m))) 
                 (transient [])
                 (range (ipc/nrows img)))
      persistent!
;      (grid-apply pix-val
;                  0 (ipc/ncols img)
;                  0 (ipc/nrows img))
;      (partition (ipc/ncols img))
;      (mapv vec)
      (#(ipc/make-image % (:type img)))
      )))
