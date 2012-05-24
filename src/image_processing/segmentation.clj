(ns image-processing.segmentation
    (:use
      [image-processing.core :only (vertical-histogram)]
      [image-processing.image :only (get-img-height get-subimage)]))


(defn vertical-histogram-autocrop
  "Options:
      :threshold (default 1)"
  #^{:arglists [[img] [img options]]}
  ([img] (vertical-histogram-autocrop img {}))
  ([img options]
   (let [threshold (or (:threshold options) 1)
         v-hist (vertical-histogram img)
         vec (partition 2 (interleave (range (count v-hist)) v-hist)) 
         x1 (first (first (filter #(> (second %) threshold) vec)))
         x2 (first (first (filter #(> (second %) threshold) (reverse vec))))]
     (if (or x1 x2)
       (get-subimage img x1 0 (- x2 x1) (get-img-height img))
       img))))
