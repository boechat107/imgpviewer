(ns image-processing.transformations
    (:use
      [image-processing.basic-math :only (mean)]
      [image-processing.image])
    (:import
      [image_processing.image Image]))


(defn to-grayscale
  "Returns the grayscale value of the Image's pixels."
  #^{:arglists [img]}
  [img]
  {:pre [(image? img)]}
  (Image. (vec (map #(let [gv (int (mean (rest %)))] ; grayscale value
                       [(first %) gv gv gv])  
                    (:points img))) 
          (:width img)))


(defn to-binary
  "Returns the binarized value of the Image's pixels.  If pixel < threshold, then
  pixel=BLACK (0) else pixel=WHITE (255).
  It uses get-grayscale-values internally."
  #^{:arglists [[img] [img threshold]]}
  ([img] (to-binary img 127))
  ([img threshold]
   (Image. (vec (map #(if (< (second %) threshold)
                        [(first %) 0 0 0] 
                        [(first %) 255 255 255])
                     (:points (to-grayscale img))))
           (:width img))))


