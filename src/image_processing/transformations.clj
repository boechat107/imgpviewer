(ns image-processing.transformations
    (:use
      [image-processing.basic-math :only (mean)]
      [image-processing.image :only (get-pixel
                                     get-height
                                     get-image-type
                                     get-pixels-of-column
                                     get-pixels-of-line
                                     get-image-abs-coords)])
    (:import
      [image_processing.image Image]))


(defn- convolution
  "General routine to convolute a mask over a Image."
  [img mask op-func val-func]
  (let [old-pixels (:pixels img)
        pixels (if (vector? old-pixels) old-pixels (vec old-pixels))
        height (get-height img)
        img-vec (Image. pixels (:width img))
        max-bound-func (fn [num] (if (>= num 0.5) 1 0))
        vals (map (fn [center & more]
                      (let [[xc yc] center] 
                        (->> (reduce op-func ;specific color type code.
                                     0.0
                                     (partition 2
                                                (interleave 
                                                  mask
                                                  (map #(get-pixel img-vec (% 0) (% 1))
                                                       (for [yn (range (dec yc) (+ 2 yc))
                                                             xn (range (dec xc) (+ 2 xc))]
                                                            [xn yn])))))
                             (val-func)))) ; specific color type code.
                  ;; Image coordinates without border.
                  (for [y (range 1 (dec height))
                        x (range 1 (dec (:width img)))]
                       [x y]))
        ;;; Joins the new pixel values with the original border of the image.
        ;; Gets the first and last lines, both without the fisrt and last element because
        ;; they are elements of the first and last columns too.
        rem-first-last (fn [seq] (->> seq (rest) (drop-last)))
        first-line (rem-first-last (get-pixels-of-line img-vec 0))
        last-line (rem-first-last (get-pixels-of-line img-vec (dec height)))
        vals-lines (partition (- (:width img) 2) vals)
        ;; Concatenates the lines, new ones and the borders of the original image.
        with-lines (concat [first-line] vals-lines [last-line])
        first-col (get-pixels-of-column img-vec 0)
        last-col (get-pixels-of-column img-vec (dec (:width img-vec)))
        vals-cols (partition (get-height img) (apply interleave with-lines))
        ;; Concatenates the columns.
        with-cols (concat [first-col] vals-cols [last-col])]
    (get-image-abs-coords (apply interleave with-cols) (:width img))))


;TODO: optimize performance
(defmulti convolve
          "Convolute a 3x3 mask over a Image. The mask must be a simple vector with
           9 real values."
          (fn [img mask] 
              (get-image-type img)))


(defmethod convolve :bw
  [img mask]
  (let [op-func (fn [cum mask-bw]
                    (+ cum (* (first mask-bw) (:bw (second mask-bw)))))
        val-func (fn [val]
                     (if (>= val 0.5) {:bw 1} {:bw 0}))]
    (convolution img mask op-func val-func)))


(defn erode
  "Erodes a Image, a basic operation in the area of the mathematical morphology.
   http://homepages.inf.ed.ac.uk/rbf/HIPR2/erode.htm

   The corner and edge values of the mask can be specified. The default values are 0.2."
   ([img] (erode img 0.2 0.2))
   ([img corner edge]
    (let [mask [corner  edge    corner
                edge    1.0     edge
                corner  edge    corner]]
      (convolve img mask))))

