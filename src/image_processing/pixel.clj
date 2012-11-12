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
      (throw (IllegalArgumentException. "Pixel is not ':argb', nor ':gray', wtf should i do? hehe")))))

(defn color-distance
  "Calculates the norm-2 distance of pixels"
  [pixel1 pixel2]
  
  {:pre [(= (pix-type pixel1) (pix-type pixel2))
           (pix-type pixel1)]}
  (let [type (pix-type pixel1)]
    (case type
      :argb (Math/sqrt (apply + (map #(Math/pow % 2)
                                    (map - (map pixel1 [:r :g :b])
                                           (map pixel2 [:r :g :b])))))
      :gray (Math/abs (- (:gray pixel1) (:gray pixel2)))
      :bw (Math/abs (- (:bw pixel1) (:bw pixel2))))))

(defn WHITE [type]
  (case type
    :argb {:a 255 :r 255 :g 255 :b 255}
    :gray {:gray 255}
    :bw {:bw 1}
    (throw (IllegalArgumentException. "Unknown image type"))))

(defn BLACK [type]
  (case type
    :argb {:a 255 :r 0 :g 0 :b 0}
    :gray {:gray 0}
    :bw {:bw 0}
    (throw (IllegalArgumentException. "Unknown image type"))))

(defn RED [type]
  (case type
    :argb {:a 255 :r 255 :g 0 :b 0}
    (throw (IllegalArgumentException. "Unknown image type"))))