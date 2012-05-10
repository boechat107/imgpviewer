(ns image-processing.core
    (:use 
      [image-processing.image] 
      [image-processing.basic-math :only (square mean)])
    (:import
      (java.lang Math)
      (javax.imageio ImageIO)
      (java.io File)
      (java.awt.image BufferedImage)
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


(defn get-rgb-only
  "Returns a vec with [r g b] if COLOR is [a r g b], or returns COLOR if it already is
      just [r g b]."
  #^{:arglists [color]}
  [color]
  (if (> (count color) 3) (subvec color 1) color))


(defn convert-buffImg-to-image
  "Returns a Image from a BufferedImage."
  #^{:arglists [buffered-image]}
  [buffered-image]
  (let [argb-values (reduce #(conj %1 (get-argb buffered-image %2))
                            []
                            (get-img-coords buffered-image))]
    (Image. argb-values (.getWidth buffered-image))))


(defn convert-image-to-buffImg
  "Doc"
  #^{:arglists [img]}
  [img]
  (let [height (/ (count (:points img)) (:width img))
        buff-img (BufferedImage. (:width img) 
                                 height
                                 BufferedImage/TYPE_INT_ARGB)]
    (doseq [[x y] (get-img-coords buff-img)]
      (set-argb buff-img [x y] (get-point img x y)))))


(defn euclidian-argb-distance
  "Euclidian distance between two [a r g b] colors."
  [color1 color2]
  (Math/sqrt (reduce + (map #(square (- %1 %2))
                            (get-rgb-only color1)
                            (get-rgb-only color2)))))


(defn get-grayscale-values
  "Returns a lazy sequence of the grayscale value of the image's pixels."
  #^{:arglists [img]}
  [img]
  (map #(int (mean (rest (get-argb img %)))) (get-img-coords img)))


(defn get-binarized-values
  "Returns a lazy sequence of the binarized value of the image's pixels.
      If pixel < threshold, then pixel=BLACK (0) else pixel=WHITE (255)."
  #^{:arglists [img threshold]}
  [img threshold]
  (map #(if (< % threshold) 0 255) (get-grayscale-values img)))


;(defn vertical-histogram
;  "Doc"
;  #^{:arglists [img]}
;  [img]
;  )
;  TODO: partition & interleave

;(def img-path "/home/boechat/Dropbox/Documents/Coding/Hough_transform/straight_lines.png")
;(def buffered-image (ImageIO/read (File. img-path)))
