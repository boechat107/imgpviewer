(ns image-processing.processing
  (:require 
    [image-processing.core-new :as ipc]
    [incanter.core :as ic]
    )
  )

(defn argb-to-gray
  "Returns a new Image whose color space is the grayscale.
  Reference:
  http://en.wikipedia.org/wiki/Grayscale"
  [img]
  {:pre [(ipc/color-type? img)]}
  ;; Todo: Just two types of color spaces are assumed, argb and rgb.
  (let [[rm gm bm] (if (= :argb (:type img))
                     (rest (:channels img))
                     (:channels img))]
    (-> (ic/matrix-map #(+ (* 0.2126 %1) (* 0.7152 %2) (* 0.0722 %3))
                       rm gm bm)
        flatten
        (ipc/make-image (ipc/ncols img) :gray))))

(defn gray-to-argb
  "Repeats the only grayscale channel for each color channel and returns a new ARGB
  Image."
  [img]
  {:pre [(= :gray (:type img))]}
  (let [gray-ch (first (:channels img))]
    (ipc/make-image (conj (repeat 3 gray-ch)
                          (ic/matrix 255 (ipc/nrows img) (ipc/ncols img)))
                    :argb)))

(defn grid-apply
  "Returns a sequence resulting from the application of the function f to each 
  value of the grid built with the rectangle x-min, x-max, y-min, y-max."
  [f x-min x-max y-min y-max]
  (for [y (range y-min y-max), x (range x-min x-max)]
    (f x y)))

(defn convolve
  [channels mask]
  ;; todo: speed up with discrete Fourier transform.
  ;; variable mask size.
  (let [nr (ic/nrow (first channels))
        nc (ic/ncol (first channels))
        real-xy (fn [rc c m] 
                  ;; Returns c if it is between the boundaries of the image. 
                  (if (or (neg? c) (== c m)) rc c))
        kernel (fn [mat x y] 
                 (->> (for [ky (range (dec y) (+ 2 y)),
                            kx (range (dec x) (+ 2 x))]
                        (ic/$ (real-xy y ky nr) (real-xy x kx nc) mat))
                      (map #(* %1 %2) mask)
                      ic/sum))]
    (->> (map #(grid-apply (partial kernel %)
                           0 nc 0 nr)
              channels) 
      (map #(ic/matrix % nc)))))

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
