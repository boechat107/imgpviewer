(ns image-processing.core
    (:use 
      [image-processing.image :only (get-image-type get-height)]
      [image-processing.basic-math :only (square mean)])
    (:import
      [image_processing.image Image]
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
  "Get the pixel {:x :y :a :r :g :b} color of a pixel [x y] of IMG."
  [img coord]
  (let [[x y] coord]
    (assoc (intcolor-to-argb (.getRGB img x y)) :x x :y y)))


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
    (doseq [pixel (:pixels img)]
      (set-buffImg-argb buff-img
                [(:x pixel) (:y pixel)]
                [(:a pixel) (:r pixel) (:g pixel) (:b pixel)]))
    buff-img))

(defmethod convert-image-to-buffImg :gray
  [img]
  (let [buff-img (create-empty-buffImg (:width img) (get-height img))]
    (doseq [pixel (:pixels img)]
      (let [gray (:gray pixel)] 
        (set-buffImg-argb buff-img
                          [(:x pixel) (:y pixel)]
                          [255 gray gray gray])))
    buff-img))

(defmethod convert-image-to-buffImg :bw
  [img]
  (let [buff-img (create-empty-buffImg (:width img) (get-height img))]
    (doseq [pixel (:pixels img)] 
      (let [bw (* 255 (:bw pixel))] 
        (set-buffImg-argb buff-img
                          [(:x pixel) (:y pixel)]
                          [255 bw bw bw])))
    buff-img))


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
   {:pre [(= :gray (get-image-type img))]}
   (Image. (map #(assoc {:x (:x %) :y (:y %)}
                        :bw (if (< (:gray %) threshold) 0 1))
                (:pixels img)) 
           (:width img))))


(defn- histogram
  "Basic operations to calculate the values of bins for histograms.
  If ARG1 is the image's height and ARG2 is the image's width, the vertical histogram
  is returned.
  If ARG1 is the image's width and ARG2 is the image's height, the horizontal histogram
  is returned."
  [pixels arg1 arg2]
  (map (fn [col-or-lin & args]
                (count (filter #(zero? (:bw %)) col-or-lin)))
            (partition arg1
                       (apply interleave (partition arg2 pixels)))))


(defn vertical-histogram
  "Returns a vector where each element represents the number of black pixels on
  each column of a BW image."
  [img]
  (histogram (:pixels img) (get-height img) (:width img)))


(defn horizontal-histogram
  "Returns a vector where each element represents the number of black pixels on
  each line of a BW image."
  [img]
  (histogram (:pixels img) (:width img) (get-height img)))
