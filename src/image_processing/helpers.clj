(ns image-processing.helpers 
  (:require 
    [image-processing.core-new :as c]
    [image-processing.processing :as pr]
    [seesaw.core :as w]
    )
  (:import 
    [java.io File]
    [javax.imageio ImageIO]
    [java.awt.image BufferedImage]  
    )
  )

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn argb<-intcolor
  "Convert the 32 bits color to ARGB. It returns a vector [a r g b]."
  [color]
  (vector 
    (bit-and (bit-shift-right color 24) 0xff) 
    (bit-and (bit-shift-right color 16) 0xff) 
    (bit-and (bit-shift-right color 8) 0xff) 
    (bit-and color 0xff)))

(defn r<-intcolor 
  "Returns the red value from a ARGB integer."
  ^long [^long color]
  (bit-and (bit-shift-right color 16) 0xff))

(defn g<-intcolor 
  "Returns the green value from a ARGB integer."
  ^long [^long color]
  (bit-and (bit-shift-right color 8) 0xff))

(defn b<-intcolor 
  "Returns the blue value from a ARGB integer."
  ^long [^long color]
  (bit-and color 0xff))

(defn intcolor<-argb
  "Converts the components ARGB to a 32 bits integer color."
  [a r g b]
  (bit-or (bit-shift-left (int a) 24)
          (bit-or (bit-shift-left (int r) 16)
                  (bit-or (bit-shift-left (int g) 8) (int b)))))

(defn get-raster-array
  "Returns the primitive array of a BufferedImage."
  [^BufferedImage buff]
  (.getDataElements (.getRaster buff) 0 0 (.getWidth buff) (.getHeight buff) nil))

(defn load-file-image
  "Returns a RGB Image from a file image."
  [^String filepath]
  (let [buff (ImageIO/read (File. filepath))
        nr (.getHeight buff)
        nc (.getWidth buff)
        img (c/new-image nr nc :rgb)
        [rch gch bch] (:mat img)]
    (dotimes [x nc]
      (dotimes [y nr]
        (let [idx (+ x (* y nc))
              int-pix (.getRGB buff x y)]
          (c/set-pixel! rch idx (r<-intcolor int-pix))
          (c/set-pixel! gch idx (g<-intcolor int-pix))
          (c/set-pixel! bch idx (b<-intcolor int-pix)))))
    img))

(defn to-buffered-image
  "Converts an ARGB Image to a BufferedImage."
  [img]
  {:pre [(c/image? img)]}
  (let [h (c/nrows img)
        w (c/ncols img)
        buff (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
        rgb-img (if (= (:type img) :gray) (pr/gray-to-rgb img) img)
        [rch gch bch] (:mat rgb-img)]
    (dotimes [y h]
      (dotimes [x w]
        (let [idx (+ x (* y w))]
          (->> (intcolor<-argb 255 
                               (c/get-pixel rch idx) 
                               (c/get-pixel gch idx) 
                               (c/get-pixel bch idx))
               (.setRGB buff x y)))))
    buff))

(defn view 
  "Shows the images on a grid-panel window."
  [& imgs]
  (let [buff-imgs (map #(if (instance? java.awt.image.BufferedImage %)
                          %
                          (to-buffered-image %))
                       imgs)
        grid (w/grid-panel
               :border 5
               :hgap 10 :vgap 10
               :columns (min 6 (max 1 (count imgs))) 
               :items (map #(w/label :icon %) buff-imgs))]
    (-> (w/frame :title "Image Viewer" 
                 :content grid)
        w/pack!
        w/show!)))
