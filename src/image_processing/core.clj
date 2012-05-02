(ns image-processing.core
    (:import
      (java.lang Math)
      (javax.imageio ImageIO)
      (java.io File)
      (java.awt.image BufferedImage))
    (:use (incanter core charts stats)))


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


(defn euclidian-argb-distance
  "Euclidian distance between two [r g b] colors."
  [color1 color2]
  (sqrt (sum-of-squares (map #(- %1 %2) color1 color2))))


(defn get-img-coords
  "Returns a sequence of all coordinates of the image."
  #^{:arglists [img]}
  [img]
  (for [y (range (.getHeight img))
        x (range (.getWidth img))]
       [x y]))


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

(def img-path "/home/boechat/Dropbox/Documents/Coding/Hough_transform/straight_lines.png")
(def buffered-image (ImageIO/read (File. img-path)))
