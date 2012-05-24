(ns image-processing.core
    (:use 
      [image-processing.image]
      [image-processing.basic-math :only (square)])
    (:import
      [java.awt.image BufferedImage]
      [image_processing.image Image]))


(defn intcolor-to-argb
  "Convert the 32 bits color to ARGB. It returns a vector [A R G B]."
  [color]
  [(bit-and (bit-shift-right color 24) 0xff)
   (bit-and (bit-shift-right color 16) 0xff)
   (bit-and (bit-shift-right color 8) 0xff)
   (bit-and color 0xff)])


(defn get-argb
  "Get the [a r g b] color of a pixel [x y] of IMG."
  [img coord]
  (let [[x y] coord]
    (intcolor-to-argb (.getRGB img x y))))


(defn set-argb
  "Set the ARGB color of a pixel COORD of the image IMG."
  #^{:arglists [img coord argb-color]}
  [img coord argb-color]
  (let [[x y] coord
        [a r g b] argb-color
        int-color (bit-or (bit-shift-left a 24)
                          (bit-or (bit-shift-left r 16)
                                  (bit-or (bit-shift-left g 8) b)))]
    (.setRGB img x y int-color)))


(defn get-img-coords
  "Returns a sequence of all coordinates of the image."
  #^{:arglists [img]}
  [img]
  (for [y (range (.getHeight img))
        x (range (.getWidth img))]
       [x y]))


(defn convert-buffImg-to-image
  "Returns a Image from a BufferedImage."
  #^{:arglists [buffered-image]}
  [buffered-image]
  (let [argb-values (vec (map #(get-argb buffered-image %)
                         (get-img-coords buffered-image)))]
    (Image. argb-values (.getWidth buffered-image))))


(defn convert-image-to-buffImg
  "Returns a BufferedImage from a Image."
  #^{:arglists [img]}
  [img]
  (let [height (/ (count (:points img)) (:width img))
        buff-img (BufferedImage. (:width img) 
                                 height
                                 BufferedImage/TYPE_INT_ARGB)]
    (doseq [[x y] (get-img-coords buff-img)]
      (set-argb buff-img [x y] (get-point img x y)))
    buff-img))


(defn- histogram
  "Basic operations to calculate the values of bins for histograms.
  If ARG1 is the image's height and ARG2 is the image's width, the vertical histogram
  is returned.
  If ARG1 is the image's width and ARG2 is the image's height, the horizontal histogram
  is returned."
  #^{:arglists [img arg1 arg2]}
  [img arg1 arg2]
  (vec (map (fn [col-or-lin & args]
                (count (filter #(zero? (second %)) col-or-lin)))
            (partition arg1
                       (apply interleave (partition arg2 (:points img)))))))


(defn vertical-histogram
  "Returns a vector where each element represents the number of black pixels on
  each column of the Image."
  #^{:arglists [img]}
  [img]
  (let [height (/ (count (:points img)) (:width img))]
    (histogram img height (:width img))))


(defn horizontal-histogram
  "Returns a vector where each element represents the number of black pixels on
  each line of the Image."
  #^{:arglists [img]}
  [img]
  (let [height (/ (count (:points img)) (:width img))]
    (histogram img (:width img) height)))
