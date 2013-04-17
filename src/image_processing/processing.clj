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
        ic/matrix
        (ipc/make-image  :gray))))

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
        real-xy (fn [c m] 
                  ;; Returns c if it is between the boundaries of the image. 
                  (min (dec m) (max 0 c)))
        kernel (fn [mat x y] 
                 ;; Apply the mask on a pixel given by [x,y] and its neighbor pixels.
                 (->> (for [ky (range (dec y) (+ 2 y)),
                            kx (range (dec x) (+ 2 x))]
                        (ic/$ (real-xy ky nr) (real-xy kx nc) mat))
                      (map #(* %1 %2) mask)
                      ic/sum
                      (min 255)
                      (max 0)))]
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
