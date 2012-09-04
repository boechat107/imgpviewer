(ns image-processing.core
    (:use 
      [image-processing.image :only (get-image-type
                                     get-height
                                     get-image-type
                                     get-pixels-of-column
                                     get-pixels-of-line
                                     get-image-abs-coords
                                     get-pixel)]
      [image-processing.basic-math :only (square mean)])
    (:import
      [image_processing.image Image]
      [javax.imageio ImageIO]
      [java.io File]
      [java.awt.image BufferedImage]))


(defn create-empty-buffImg
  "Creates a empty BufferedImage."
  [width height]
  (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))


(defn intcolor-to-argb
  "Convert the 32 bits color to ARGB. It returns a map {:a :r :g :b}."
  [color]
  {:a (bit-and (bit-shift-right color 24) 0xff) 
  :r (bit-and (bit-shift-right color 16) 0xff) 
  :g (bit-and (bit-shift-right color 8) 0xff) 
  :b (bit-and color 0xff)})


(defn get-buffImg-pixel
  "Get the pixel {:a :r :g :b} color of a pixel [x y] of IMG."
  [img coord]
  (let [[x y] coord]
    (intcolor-to-argb (.getRGB img x y))))


(defn set-buffImg-argb
  "Set the [a r g b] color of a pixel COORD of the image IMG."
  [img coord argb-color]
  (let [[x y] coord
        [a r g b] argb-color 
        int-color (bit-or (bit-shift-left a 24)
                          (bit-or (bit-shift-left r 16)
                                  (bit-or (bit-shift-left g 8) b)))]
    (.setRGB img x y int-color)))


(defn get-img-coords
  "Returns a sequence [x y] of all coordinates of the image."
  [img]
  (for [y (range (.getHeight img))
        x (range (.getWidth img))]
       [x y]))


(defn convert-buffImg-to-image
  "Returns a Image (with a lazy sequence of pixels) from a BufferedImage."
  [buffered-image]
  (let [argb-values (map #(get-buffImg-pixel buffered-image %)
                         (get-img-coords buffered-image))]
    (Image. argb-values (.getWidth buffered-image))))


(defmulti convert-image-to-buffImg
          "Converts a image structure to a BufferedImage object."
          get-image-type)

(defmethod convert-image-to-buffImg :argb
  [img]
  (let [buff-img (create-empty-buffImg (:width img) (get-height img))]
    (doseq [[x y] (get-img-coords buff-img)]
      (let [pixel (get-pixel img x y)]
        (set-buffImg-argb buff-img [x y] [(:a pixel) (:r pixel) (:g pixel) (:b pixel)])))
    buff-img))




(defmethod convert-image-to-buffImg :gray
  [img]
  (let [buff-img (create-empty-buffImg (:width img) (get-height img))]
    (doseq [[x y] (get-img-coords buff-img)]
      (let [gray (-> (get-pixel img x y) :gray)]
        (set-buffImg-argb buff-img [x y] [255 gray gray gray])))
    buff-img))

(defmethod convert-image-to-buffImg :bw
  [img]
  (let [buff-img (create-empty-buffImg (:width img) (get-height img))]
    (doseq [[x y] (get-img-coords buff-img)]
      (let [bw (-> (get-pixel img x y) :bw (* 255))]
        (set-buffImg-argb buff-img [x y] [255 bw bw bw])))
    buff-img))


(defn save-buffImg
  "Saves the BufferedImage as a PNG file."
  [buff-img path-name]
  (ImageIO/write buff-img "png" (File. path-name)))


(defn load-file-buffImg
  "Load a image file as a BufferedImage."
  [path-name]
  (ImageIO/read (File. path-name)))


(defn to-grayscale
  "Returns the grayscale image (with a lazy sequence of pixels) of a argb image."
  [img]
  {:pre [(= :argb (get-image-type img))]}
  (Image. (map #(let [rgb [(:r %) (:g %) (:b %)]]
                  {:x (:x %) :y (:y %) :gray (int (mean rgb))})
               (:pixels img)) 
          (:width img)))


(defn to-binary
  "Returns the binarized value of the Image's pixels (with a lazy sequence of pixels).
   If pixel < threshold, then
   pixel=BLACK (0) else pixel=WHITE (1)."
  ([img] (to-binary img 127))
  ([img threshold]
   (case (get-image-type img)
     :argb (to-binary (to-grayscale img) threshold)
     :bw img
     :gray (Image. (map #(assoc {:x (:x %) :y (:y %)}
                                :bw (if (< (:gray %) threshold) 0 1))
                        (:pixels img)) 
                   (:width img)))))


(defn vertical-histogram
  "Returns a vector where each element represents the number of black pixels on
  each column of a BW image."
  [img]
  (map 
    (fn [col & args] (count (filter #(zero? (:bw %)) col)))
    (partition (get-height img)
               (apply interleave (partition (:width img) (:pixels img))))))


(defn horizontal-histogram
  "Returns a vector where each element represents the number of black pixels on
  each line of a BW image."
  [img]
  (map 
    (fn [lin & args] (count (filter #(zero? (:bw %)) lin)))
    (partition (:width img) (:pixels img))))


(defn- convolution
  "General routine to convolute a mask over a Image."
  [img mask op-func val-func]
  (let [old-pixels (:pixels img)
        pixels (if (vector? old-pixels) old-pixels (vec old-pixels))
        height (get-height img)
        img-vec (Image. pixels (:width img))
        max-bound-func (fn [num] (if (>= num 0.5) 1 0))
        ;TODO: just ignore pixels of the borders.
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
