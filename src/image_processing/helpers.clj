(ns image-processing.helpers 
  (:require 
    [image-processing.core-new :as ipc]
    [image-processing.processing :as pr]
    [seesaw.core :as w]
    )
  (:import 
    [java.io File]
    [javax.imageio ImageIO]
    [java.awt.image BufferedImage]  
    )
  )

(defn argb<-intcolor
  "Convert the 32 bits color to ARGB. It returns a map {:a :r :g :b}."
  [color]
  {:a (bit-and (bit-shift-right color 24) 0xff) 
  :r (bit-and (bit-shift-right color 16) 0xff) 
  :g (bit-and (bit-shift-right color 8) 0xff) 
  :b (bit-and color 0xff)})

(defn intcolor<-argb
  "Converts the components ARGB to a 32 bits integer color."
  [argb]
  (let [[a r g b] (map int argb)]
    (-> (bit-or (bit-shift-left a 24)
                (bit-or (bit-shift-left r 16)
                        (bit-or (bit-shift-left g 8) b)))
        (.intValue))))

(defn load-file-image
  "Returns a RGB Image from a file image."
  [filepath]
  (let [buff (ImageIO/read (File. filepath))
        ncols (.getWidth buff)
        buff-data (for [y (range (.getHeight buff)), x (range ncols)]
                    (.getRGB buff x y))]
    (-> (map (comp (juxt :r :g :b) argb<-intcolor) buff-data)
        (make-image ncols argb))))

(defn to-buffered-image
  "Converts an ARGB Image to a BufferedImage."
  [img]
  {:pre [(image? img)]}
  (let [h (nrows img)
        w (ncols img)
        buff (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
        pix-val (if (= :gray (:type img))
                  ;; The grayscale value is used for the three channels (RGB) and the
                  ;; transparency is set to 255.
                  ;; todo: rewrite 
                  (fn [x y] (conj (->> (get-xy img x y) first (repeat 3))
                                  255))
                  (fn [x y] (conj (get-xy img x y) 255)))]
    (doseq [[x y] (for [x (range w), y (range h)] [x y])]
      (->> (pix-val x y) 
           intcolor<-argb
           (.setRGB buff x y)))
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