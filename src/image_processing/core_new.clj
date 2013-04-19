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

(defrecord Image [mat type])

(defn image?
  [obj]
  (instance? Image obj))

(defn valid-type?
  [type]
  (some #(= type %) [:argb :rgb :gray]))

(defn mat?
  [obj]
  (and (vector? obj) (every? vector? obj)))

(defn color-type?
  [img]
  (some #(= % (:type img)) [:argb :rgb]))

(defn gray-type?
  [img]
  (= :gray (:type img)))

(defn argb-type?
  [img]
  (= :argb (:type img)))

;;; 
;;; Functions that depends of the library used to represent the image information.
;;; 

(defn nrows
  "Returns the number of rows of an Image."
  [^Image img]
  (count (:mat img)))

(defn ncols
  "Returns the number of rows of an Image."
  [^Image img]
  (count (first (:mat img))))

(defn make-image
  "Returns an instance of Image for a given image data, its number of columns of
  pixels and the color space of the image. 
  data-mat must be something like this:
  [[    ]
   [    ]
   ...
   [    ]]
  data-array must be just a list of vectors (for RGB images) or of numbers, sorted by
  their position in the image.
  0 +----------------+ w - 1
    |                |
    |                |
    +----------------+ h*w - 1"
  ([data-mat type]
   {:pre [(valid-type? type) (vector? data-mat) (every? vector? data-mat)]}
   (Image.  data-mat type))
  ([data-array ncols type]
   {:pre [(valid-type? type) (coll? data)]}
   (letfn [(constructor [m] (Image. m type))]
     (->> data-array 
       (partition ncols)
       (map vec)
       vec
       constructor))))

(defn get-xy
  "Returns the value of the representation of pixel [x, y], where x increases 
  for columns."
  [img x y]
  (get-in (:mat img) [y x]))

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
