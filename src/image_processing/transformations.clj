(ns image-processing.transformations
    (:use
      [image-processing.core :only (convolve
                                    create-empty-buffImg
                                    set-buffImg-argb
                                    get-img-coords)]
      [image-processing.basic-math :only (mean)])
    (:import
      [java.awt Graphics]
      [java.awt.geom AffineTransform Rectangle2D]
      [java.awt.image AffineTransformOp BufferedImage]
      [image_processing.image Image]))


(defn scale-one-dim
  "Scales a BufferedImage to size WIDTHxHEIGHT and returns a BufferedImage."
  [buff-img width height]
  (let [w (.getWidth buff-img)
        h (.getHeight buff-img)
        temp-scale-x (/ width w)
        temp-scale-y (/ height h)
        [scale-x scale-y] (if (< (* w temp-scale-x) (+ height 0.5))
                            [temp-scale-x temp-scale-x]
                            [temp-scale-y temp-scale-y])
        afop (AffineTransformOp. (AffineTransform/getScaleInstance scale-x scale-y)
                                 (AffineTransformOp/TYPE_BILINEAR))
        rect (.getBounds2D afop buff-img)
        r-width (max (int (- (.getMaxX rect) (.getMinX rect)))
                    1)
        r-height (max (int (- (.getMaxY rect) (.getMinY rect)))
                      1)
        temp-buff (.filter afop buff-img nil)
        output (create-empty-buffImg width height)]
    (doseq [y (range (.getHeight buff-img))
            x (range (.getWidth buff-img))]
           (set-buffImg-argb output [x y] [255 255 255 255]))
    (let [g (.getGraphics output)] 
      (.drawImage g
                  temp-buff
                  (/ (- width r-width) 2)
                  (/ (- height r-height) 2)
                  nil)
      (.dispose g))
    output))


(defn erode
  "Erodes a Image, a basic operation in the area of the mathematical morphology.
   http://homepages.inf.ed.ac.uk/rbf/HIPR2/erode.htm
   The corner and edge values of the mask can be specified. The default values are 0.2."
   ([img] (erode img 0.2 0.2))
   ([img corner edge]
    (let [mask [corner  edge    corner
                edge    1.0     edge
                corner  edge    corner]]
      (convolve img mask))))

