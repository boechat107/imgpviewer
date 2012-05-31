(ns image-processing.transformations
    (:use
      [image-processing.core :only (convolve)]
      [image-processing.basic-math :only (mean)])
    (:import
      [image_processing.image Image]))


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

