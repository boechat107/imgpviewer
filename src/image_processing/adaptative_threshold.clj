(ns image-processing.adaptative-threshold
  (:require [image-processing.image :as img]
            [image-processing.core :as imgc])

  (:import [image_processing.image Image])
  )


(def timg (-> "/home/usuario/clojure/imgpviewer/test/adaptativethresholding.png"
              imgc/load-file-Img))

(def timg-s (Image. (map #(hash-map :gray %) (range 4)) 2))

(defn integral-img [img]
  {:pre [(= (img/get-image-type img) :gray)]}
  (let [w (:width img)
        h (img/get-height img)]
    (first (reduce (fn [[result sum] [x y]]
                     (let [sum (if (= 0 y) 0 sum)
                           intensity (:gray (img/get-pixel img x y))]
                       (vector
                        (assoc result [x y] (+ intensity sum (get result [(dec x) y] 0)))
                        (+ sum intensity))))
                   [{} 0]
                   (for [i (range 0 w)
                         j (range 0 h)]
                     [i j])))))



(defn threshold
  ([img]
     (threshold img 0.7))
  ([img threshold-value]
     (let [gray-img (imgc/to-grayscale img)
           w (:width img)
           h (img/get-height img)
           half-window-width (Math/round (double (/ w 8)))
           half-window-height (Math/round (double (/ h 8)))
           int-img (integral-img gray-img)]
       (Image.
        (for [j (range h) i (range w)]
          (let [x1 (max (- i half-window-width) 0)
                x2 (min (+ i half-window-width) (dec w))
                y1 (max (- j half-window-height) 0)
                y2 (min (+ j half-window-height) (dec h))
;               _ (print x1 y1 ";")
;               _ (println x2 y2 ";")
                in (-> (img/get-pixel gray-img i j) :gray)
                ct (* (- x2 x1 -1)
                      (- y2 y1 -1))
;                _ (print ct ":")
                sum (- (int-img [x2 y2] 0)
                       (int-img [x2 (dec y1)] 0)
                       (int-img [(dec x1) y2] 0)
                       (- (int-img [(dec x1) (dec y1)] 0)))
;                _ (println sum)
                ]
            (if (<= (* in  ct)
                    (* sum threshold-value))
              {:bw 0}
              {:bw 1})))
        w))))
