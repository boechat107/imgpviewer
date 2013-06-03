(ns image-processing.processing
  (:require 
    [image-processing.core-new :as c]
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
  (let [nc (c/ncols img)
        nr (c/nrows img)
        res (c/new-image nr nc :gray)
        gray (first (:mat res))
        [rch gch bch] (:mat img)]
    (c/for-img [idx img]
      (->> (* 0.2126 (c/get-ch-pixel rch idx))
           (+ (* 0.7152 (c/get-ch-pixel gch idx)))
           (+ (* 0.0722 (c/get-ch-pixel bch idx)))
           (ut/mult-aset ints gray idx)))
    res))

(defn gray-to-rgb
  "Repeats the only grayscale channel for each color channel and returns a new RGB
  Image."
  [img]
  {:pre [(= :gray (:type img))]}
  (let [nr (c/nrows img)
        nc (c/ncols img)
        gray ((:mat img) 0)
        res (c/new-image nr nc :rgb)
        [rch gch bch] (:mat res)]
    (c/for-img [idx img]
      (let [p (c/get-ch-pixel gray idx)]
        (ut/mult-aset ints rch idx p)
        (ut/mult-aset ints gch idx p)
        (ut/mult-aset ints bch idx p)))
    res))

(defn binarize
  "Returns a new Image where each pixel value is set to 0 or 255. If the original
  pixel value is below the threshold, the value is set to 0; otherwise, the value is
  set to 255."
  [img th]
  (let [th (long th)
        nr (c/nrows img)
        nc (c/ncols img)
        res (c/new-image nr nc (:type img))
        threshold (fn [^long n] (if (> n th) 255 0))]
    (dotimes [ch (c/dimension img)]
      (let [img-m ((:mat img) ch)
            res-m ((:mat res) ch)]
        (c/for-img [idx img]
          (->> (c/get-ch-pixel img-m idx)
               threshold 
               (ut/mult-aset ints res-m idx)))))
    res))

(defn apply-kernel 
  "Returns a scalar value resulting of applying a kernel mask to a [x, y] pixel and
  its neighbors."
  [img x y ch mask]
  (loop [pos (long 0), res (double 0.0)]
    (if (< pos 9)
      (recur (inc pos)
             (->> (c/get-neighbour img x y ch pos)
                  (* (aget ^doubles mask pos))
                  (+ res)))
      res)))

(defn convolve
  [img mask]
  (let [res (c/new-image (c/nrows img) (c/ncols img) (:type img))]
    (dotimes [ch (c/dimension img)]
      (dotimes [y (c/nrows img)]
        (dotimes [x (c/ncols img)]
          (->> (apply-kernel img x y ch mask)
               (c/set-pixel! res (* x y) ch))
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
    {:pre [(c/gray-type? img)]}
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
;  {:pre [(c/color-type? img)]}
;  (letfn [;; Calculates the coefficient value for a pixel.
;          (coef [c1 c2] 
;               (-> (rgb/abs-distance c1 c2)
;                   (/ (* 3 255))
;                   (#(- 1 %))
;                   (ic/pow 10)))
;          ;; Calculates the new value of the pixel [x, y] by applying a convolution.
;          (pix-val [x y]
;            (->> (get-neighbour-pixels img x y)
;                 (map #(coef (c/get-xy img x y) %))
;                 ((fn [cs] (map #(/ % (ic/sum cs)) cs)))
;                 (apply-kernel img x y)))]
;    (->> (reduce (fn [m y]
;                   (->> (reduce #(conj! %1 (pix-val %2 y))
;                                (transient [])
;                                (range (c/ncols img)))
;                        persistent!
;                        (conj! m))) 
;                 (transient [])
;                 (range (c/nrows img)))
;      persistent!
;;      (grid-apply pix-val
;;                  0 (c/ncols img)
;;                  0 (c/nrows img))
;;      (partition (c/ncols img))
;;      (mapv vec)
;      (#(c/make-image % (:type img)))
;      )))
