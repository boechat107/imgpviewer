(ns image-processing.helpers 
  (:require 
    [image-processing.core-new :as ipc]
    [image-processing.processing :as pr]
    [image-processing.utils :as ut]
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
  [color]
  (bit-and (bit-shift-right color 16) 0xff))

(defn g<-intcolor 
  "Returns the green value from a ARGB integer."
  [color]
  (bit-and (bit-shift-right color 8) 0xff))

(defn b<-intcolor 
  "Returns the blue value from a ARGB integer."
  [color]
  (bit-and color 0xff))

(defn intcolor<-argb
  "Converts the components ARGB to a 32 bits integer color."
  [a r g b]
  (int 
    (bit-or (bit-shift-left (int a) 24)
            (bit-or (bit-shift-left (int r) 16)
                    (bit-or (bit-shift-left (int g) 8) (int b))))))

(defn load-file-image
  "Returns a RGB Image from a file image."
  [^String filepath]
  (let [buff (ImageIO/read (File. filepath))
        nr (.getHeight buff)
        nc (.getWidth buff)
        img (ipc/new-image nr nc :rgb)]
    (dotimes [c nc]
      (dotimes [r nr]
        (let [int-pix (.getRGB buff c r)]
          (ipc/set-pixel! img c r 0 (r<-intcolor int-pix))
          (ipc/set-pixel! img c r 1 (g<-intcolor int-pix))
          (ipc/set-pixel! img c r 2 (b<-intcolor int-pix)))))
    img))

(defn to-buffered-image
  "Converts an ARGB Image to a BufferedImage."
  ;; todo: avoid using setRGB, use the raster data.
  [img]
  {:pre [(ipc/image? img)]}
  (let [h (ipc/nrows img)
        w (ipc/ncols img)
        buff (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
        rgb-img (if (= (:type img) :gray) (pr/gray-to-rgb img) img)]
    (dorun 
      (for [y (range h), x (range w)] 
        (->> (intcolor<-argb 255 
                             (ipc/get-pixel rgb-img x y 0)
                             (ipc/get-pixel rgb-img x y 1)
                             (ipc/get-pixel rgb-img x y 2))
             (.setRGB buff x y))))
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
