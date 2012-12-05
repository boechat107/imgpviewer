(ns image-processing.pixel
  (:use [image-processing.basic-math :only [mean]]))

(defmacro switch-pix-type [type expr-argb expr-gray expr-bw & expr-else]
  (assert (or (nil? expr-else) (= 1 (count expr-else))))
  (concat
   `(case ~type
      :argb ~expr-argb
      :gray ~expr-gray
      :bw ~expr-bw)
   expr-else))

(defn pix-type [pix]
  (cond
   (:bw pix) :bw
   (:gray pix) :gray
   (and (:r pix) (:g pix) (:b pix)) :argb
   :else nil))

(defn grayscale-value [pixel]
  (let [ptype (pix-type pixel)]
    (switch-pix-type ptype
      (apply min ((juxt :r :g :b) pixel))
      (:gray pixel)
      (* 255 (:bw pixel))
      (throw (IllegalArgumentException. "Pixel is not ':argb', nor ':gray', wtf should i do? hehe")))))


(defn color-distance
  "Calculates the norm-2 distance of pixels"
  [pixel1 pixel2]
  
  {:pre [(= (pix-type pixel1) (pix-type pixel2))
           (pix-type pixel1)]}
  (let [type (pix-type pixel1)]
    (switch-pix-type type
                     (Math/sqrt (apply + (map #(Math/pow % 2)
                                    (map - (map pixel1 [:r :g :b])
                                           (map pixel2 [:r :g :b])))))
                     (Math/abs (- (:gray pixel1) (:gray pixel2)))
                     (Math/abs (- (:bw pixel1) (:bw pixel2))))))

(defn RAND-COLOR
  ([type]
     (RAND-COLOR type [50 200]))
  ([type [lower-bound upper-bound]]
     {:pre [(<= 0 lower-bound upper-bound 255)]}
     (switch-pix-type type
                      (reduce #(assoc %1 %2 (+ lower-bound (rand-int (- upper-bound lower-bound -1)))) {:a 255} [:r :g :b])
                      {:gray (+ lower-bound (rand-int (- upper-bound lower-bound)))}
                      {:bw (rand-int 2)})))

(defn WHITE [type]
  (switch-pix-type type
                   {:a 255 :r 255 :g 255 :b 255}
                   {:gray 255}
                   {:bw 1}
                   (throw (IllegalArgumentException. "Unknown image type"))))

(defn BLACK [type]
  (switch-pix-type type
                   {:a 255 :r 0 :g 0 :b 0}
                   {:gray 0}
                   {:bw 0}
                   (throw (IllegalArgumentException. "Unknown image type"))))

(defn RED [type]
  (let [exception #(throw (IllegalArgumentException. "Unable to have RED pix of type " type))]
    (switch-pix-type type
                     {:a 255 :r 255 :g 0 :b 0}
                     exception
                     exception
                     exception)))