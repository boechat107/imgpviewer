(ns image-processing.segmentation
    (:use
      [image-processing.core :only (vertical-histogram horizontal-histogram)]
      [image-processing.image :only (get-height get-subimage)]))


(defn- get-limits-histogram-autocrop
  "Doc"
  [hist options]
  (let [threshold (or (:threshold options) 1)
        vec (partition 2 (interleave (range (count hist)) hist)) 
        l1 (first (first (filter #(> (second %) threshold) vec)))
        l2 (first (first (filter #(> (second %) threshold) (reverse vec))))]
    (if (and (not (nil? l1)) (>= l1 l2))
      [nil nil]
      [l1 l2])))


(defn vertical-histogram-autocrop
  "Removes blank columns from the left and/or right of the image.
   If all image is croped, nil is returned.
   Options:
      :threshold (default 1)"
  ([img] (vertical-histogram-autocrop img {}))
  ([img options]
   (let [[x1 x2] (get-limits-histogram-autocrop (vertical-histogram img)
                                                options)]
     (if (or x1 x2)
       (get-subimage img x1 0 (inc (- x2 x1)) (get-height img))))))


(defn horizontal-histogram-autocrop
  "Removes blank lines from the top and/or the bottom of the image.
   If all image is croped, nil is returned.
   Options:
      :threshold (default 1)"
  ([img] (horizontal-histogram-autocrop img {}))
  ([img options]
   (let [[y1 y2] (get-limits-histogram-autocrop (horizontal-histogram img)
                                                options)]
     (if (or y1 y2)
       (get-subimage img 0 y1 (:width img) (- y2 y1))))))
