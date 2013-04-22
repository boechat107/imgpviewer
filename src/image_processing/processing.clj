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
  (-> (ipc/mat-map #(vector 255 (0 %) (1 %) (2 %)) (:mat img))
      (ipc/make-image :argb)))

(defn grid-apply
  "Returns a sequence resulting from the application of the function f to each 
  value of the grid built with the rectangle x-min, x-max, y-min, y-max."
  [f x-min x-max y-min y-max]
  (for [y (range y-min y-max), x (range x-min x-max)]
    (f x y)))

(defn convolve
  [img mask]
  ;; todo: speed up with discrete Fourier transform.
  ;; variable mask size.
  (let [nr (ipc/nrows img)
        nc (ipc/ncols img)
        real-xy (fn [c m] 
                  ;; Returns c if it is between the boundaries of the image. 
                  (min (dec m) (max 0 c)))
        kernel (fn [x y] 
                 ;; Apply the mask on a pixel given by [x,y] and its neighbor pixels.
                 (->> (for [ky (range (dec y) (+ 2 y)),
                            kx (range (dec x) (+ 2 x))]
                        (to-vec 
                          (ipc/get-xy img (real-xy kx nc) (real-xy ky nr))))
                      ;; Multiplication of each pixel of the mask.
                      (map #(ut/mult %1 %2) mask)
                      (#(if (ipc/gray-type? img)
                          ;; When img is a grayscale Image, their pixels value are
                          ;; just numbers instead of a vector of numbers.
                          (->> (ic/sum %)
                               (min 255)
                               (max 0))
                          (->> (map ic/sum)
                               (map #(min % 255))
                               (map #(max %0))
                               vec)))))]
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
      (-> (convolve (:channels img) mask)
          (ipc/make-image (:type img))))))

(defn- remove-alpha
  "If the Image is ARGB, returns just the RGB channels as a list of matrices;
  otherwise, the original channels are returned."
  [img]
  (if (ipc/argb-type? img) (rest (:channels img)) (:channels img)))

(defn- include-alpha
  "If the Image is ARGB, the first channel is conjoined with given list of
  matrices."
  [img data]
  (if (ipc/argb-type? img) (conj data (first (:channels img))) data))

(defn binarize
  "Returns a new Image where each pixel value is set to 0 or 255. If the original
  pixel value is below the threshold, the value is set to 0; otherwise, the value is
  set to 255."
  [img th]
  (->> (remove-alpha img)
       (map (fn [ch]
              (ic/matrix-map #(if (> % th) 255 0) ch)))
       (map ic/matrix)
       (include-alpha img)
       (#(ipc/make-image % (:type img)))))
