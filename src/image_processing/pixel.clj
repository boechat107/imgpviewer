(ns image-processing.pixel
  (:use [image-processing.basic-math :only [mean]]))

(defn pix-type [pix]
  (cond
   (:bw pix) :bw
   (:gray pix) :gray
   (and (:r pix) (:g pix) (:b pix)) :argb
   :else nil))

(defn grayscale-value [pixel]
  (let [ptype (pix-type pixel)]
    (case ptype
      :argb (apply min ((juxt :r :g :b) pixel))
      :gray (:gray pixel)
      :bw (* 255 (:bw pixel))
      (throw (IllegalArgumentException. "Pixel is not ':argb', nor ':gray', wtf should i do? hehe"))))
  )

