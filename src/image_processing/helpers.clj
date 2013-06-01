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

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

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
  (let [get-ch (fn [c] (int (argb c)))
        a (get-ch 0)
        r (get-ch 1)
        g (get-ch 2)
        b (get-ch 3)]
    (bit-or (bit-shift-left a 24)
            (bit-or (bit-shift-left r 16)
                    (bit-or (bit-shift-left g 8) b)))))

(defn load-file-image
  "Returns a RGB Image from a file image."
  [^String filepath]
  (let [buff (ImageIO/read (File. filepath))]
    (-> (mapv (fn [y]
                (mapv (comp (juxt :r :g :b)
                            argb<-intcolor 
                            #(.getRGB buff % y)) 
                      (range (.getWidth buff))))
              (range (.getHeight buff)))
        (ipc/make-image :rgb))))

(defn to-buffered-image
  "Converts an ARGB Image to a BufferedImage."
  [img]
  {:pre [(ipc/image? img)]}
  (let [h (ipc/nrows img)
        w (ipc/ncols img)
        buff (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
        argb-img (condp = (:type img)
                   :gray (pr/gray-to-argb img)
                   :rgb (pr/rgb-to-argb img)
                   :argb img)]
    (doseq [[y row] (map-indexed #(vector %1 %2) (:mat argb-img))]
      (doseq [[x elem] (map-indexed #(vector %1 %2) row)]
        (->> elem 
             intcolor<-argb
             int
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
