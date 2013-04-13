(ns image-processing.core-new
  (:require 
    [incanter.core :as ic]
    [seesaw.core :as w]
    )
  (:import 
    [java.io File]
    [javax.imageio ImageIO]
    [java.awt.image BufferedImage]  
    )
  )

(defrecord Image [channels type])

(defn image?
  [obj]
  (instance? Image obj))

(defn valid-type?
  [type]
  (some #(= type %) [:argb :rgb :gray]))

(defn color-type?
  [img]
  (some #(= % (:type img)) [:argb :rgb]))

;;; 
;;; Functions that depends of the library used to represent the image information.
;;; 

(defn nrows
  "Returns the number of rows of an Image."
  [^Image img]
  (ic/nrow (first (:channels img))))

(defn ncols
  "Returns the number of rows of an Image."
  [^Image img]
  (ic/ncol (first (:channels img))))

(defn make-image
  "Returns an instance of Image for a given image data, its number of columns of
  pixels and the color space of the image. The data of an image is expected as a
  collection with each pixel value of the image, each row concatenated after the
  other."
  [data ncols type]
   {:pre [(valid-type? type) (or (every? coll? data) (coll? data))]}
   (letfn [(constructor [chs]
             (Image. chs type))]
     (condp = type 
       :argb (constructor (vec (map #(-> (map % data)
                                         (ic/matrix ncols))
                                    [:a :r :g :b])))
       :gray (constructor [(ic/matrix data ncols)]))))

(defn get-xy
  "Returns the value of the representation of pixel [x, y], where x increases 
  for columns."
  [img x y]
  {:pre [(every? ic/matrix? (:channels img))]}
  (->> (:channels img)
       ;; ys are the rows and xs are the columns. 
       (map #(ic/$ y x %))
       vec))

;;;
;;; End of the section of code specific for the library used to represent an image.
;;; 

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
  [filepath]
  (let [buff (ImageIO/read (File. filepath))
        ncols (.getWidth buff)
        buff-data (for [y (range (.getHeight buff)), x (range ncols)]
                    (.getRGB buff x y))]
    (-> (map argb<-intcolor buff-data)
        (make-image ncols :argb))))

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
                  (fn [x y] (conj (->> (get-xy img x y) first (repeat 3))
                                  255))
                  (fn [x y] (get-xy img x y)))]
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
