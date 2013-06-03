(ns image-processing.processing
  (:require 
    [image-processing.core-new :as ipc]
    [image-processing.utils :as ut]
    [image-processing.rgb-color :as rgb]
    [incanter.core :as ic]
    )
  )

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn rgb-to-gray
  "Returns a new Image whose color space is the grayscale.
  Reference:
  http://en.wikipedia.org/wiki/Grayscale"
  [img]
  {:pre [(= :rgb (:type img))]}
  (let [nc (ipc/ncols img)
        nr (ipc/nrows img)
        res (ipc/new-image nr nc :gray)
        gray (first (:mat res))
        [rch gch bch] (:mat img)]
    (ipc/for-img [idx img]
      (->> (* 0.2126 (ut/mult-aget ints rch idx))
           (+ (* 0.7152 (ut/mult-aget ints gch idx)))
           (+ (* 0.0722 (ut/mult-aget ints bch idx)))
           (ut/mult-aset ints gray idx)))
    res))

(defn gray-to-rgb
  "Repeats the only grayscale channel for each color channel and returns a new RGB
  Image."
  [img]
  {:pre [(= :gray (:type img))]}
  (let [nr (ipc/nrows img)
        nc (ipc/ncols img)
        gray ((:mat img) 0)
        res (ipc/new-image nr nc :rgb)
        [rch gch bch] (:mat res)]
    (ipc/for-img [idx img]
      (let [p (ut/mult-aget ints gray idx)]
        (ut/mult-aset ints rch idx p)
        (ut/mult-aset ints gch idx p)
        (ut/mult-aset ints bch idx p)))
    res))

(defn binarize
  "Returns a new Image where each pixel value is set to 0 or 255. If the original
  pixel value is below the threshold, the value is set to 0; otherwise, the value is
  set to 255."
  [img th]
  (let [res (ipc/new-image (ipc/nrows img) (ipc/ncols img) (:type img))
        threshold (fn [n] (if (> n th) 255 0))]
    (dotimes [ch (ipc/dimension img)]
      (ipc/grid-apply 
        #(->> (ipc/get-pixel img %1 %2 ch)
              threshold 
              (ipc/set-pixel! res %1 %2 ch))
        img))
    res))

(defn apply-kernel 
  "Returns a scalar value resulting of applying a kernel mask to a [x, y] pixel and
  its neighbors."
  [img x y ch mask]
  (loop [pos (long 0), res (double 0.0)]
    (if (< pos 9)
      (recur (inc pos)
             (->> (ipc/get-neighbour img x y ch pos)
                  (* (aget ^doubles mask pos))
                  (+ res)))
      res)))

(defn convolve
  [img mask]
  (let [res (ipc/new-image (ipc/nrows img) (ipc/ncols img) (:type img))]
    (dotimes [ch (ipc/dimension img)]
      (dotimes [y (ipc/nrows img)]
        (dotimes [x (ipc/ncols img)]
          (->> (apply-kernel img x y ch mask)
               (ipc/set-pixel! res (* x y) ch))
          )
        )
      )
    )
  )

(defn erode
  "Erodes a Image, a basic operation in the area of the mathematical morphology.
   http://homepages.inf.ed.ac.uk/rbf/HIPR2/erode.htm
   The corner and edge values of the mask can be specified. The default values are 0.2."
   ([img] (erode img 0.2 0.2))
   ([img corner edge]
    {:pre [(ipc/gray-type? img)]}
    (let [mask (double-array [corner  edge    corner
                edge    1.0     edge
                corner  edge    corner])]
      (convolve img mask))))


;
;(defn smoothing
;  "Returns a new Image resulting of the application of a edge preserving smoothing on
;  the given Image.
;  -----------
;  Reference: 
;  Nikolaou, N., & Papamarkos, N. (2009). Color reduction for complex document images.
;  International Journal of Imaging Systems and Technology, 19(1), 14–26.
;  doi:10.1002/ima.20174"
;  [img]
;  {:pre [(ipc/color-type? img)]}
;  (letfn [;; Calculates the coefficient value for a pixel.
;          (coef [c1 c2] 
;               (-> (rgb/abs-distance c1 c2)
;                   (/ (* 3 255))
;                   (#(- 1 %))
;                   (ic/pow 10)))
;          ;; Calculates the new value of the pixel [x, y] by applying a convolution.
;          (pix-val [x y]
;            (->> (get-neighbour-pixels img x y)
;                 (map #(coef (ipc/get-xy img x y) %))
;                 ((fn [cs] (map #(/ % (ic/sum cs)) cs)))
;                 (apply-kernel img x y)))]
;    (->> (reduce (fn [m y]
;                   (->> (reduce #(conj! %1 (pix-val %2 y))
;                                (transient [])
;                                (range (ipc/ncols img)))
;                        persistent!
;                        (conj! m))) 
;                 (transient [])
;                 (range (ipc/nrows img)))
;      persistent!
;;      (grid-apply pix-val
;;                  0 (ipc/ncols img)
;;                  0 (ipc/nrows img))
;;      (partition (ipc/ncols img))
;;      (mapv vec)
;      (#(ipc/make-image % (:type img)))
;      )))
