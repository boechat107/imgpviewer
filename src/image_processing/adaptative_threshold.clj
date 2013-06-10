(ns image-processing.adaptative-threshold
  (:require [image-processing.image :as img]
            [image-processing.core :as imgc])

  (:import [image_processing.image Image])
  (:use [image-processing.charts])
  )

(set! *warn-on-reflection* true)

(def timg (-> "test/adaptativethresholding.png"
              imgc/load-file-Img))

(def gray-timg (imgc/to-grayscale timg))

(def cimg (-> "resources/IMG_0019_redim2.JPG"
             imgc/load-file-Img))

(def timg-s (Image. (map #(hash-map :gray %) (range 4)) 2))

(def gray-timg-s (Image. (map #(hash-map :gray %) (range 4)) 2))

(def bw-timg-s (Image. (map #(hash-map :bw %) (repeat 4 1)) 2))

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
           int-img (integral-img gray-img)
           _ (type (:pixels gray-img))]
       (Image.
        (vec (for [j (range h) i (range w)]
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
                           sum (- (get int-img [x2 y2] 0)
                                  (get int-img [x2 (dec y1)] 0)
                                  (get int-img [(dec x1) y2] 0)
                                  (- (get int-img [(dec x1) (dec y1)] 0)))
                                        ;                _ (println sum)
                           ]
                       (if (<= (* in  ct)
                               (* sum threshold-value))
                         {:bw 0}
                         {:bw 1}))))
        w))))


(defn aintegral-img [img]
  {:pre [(= (img/get-image-type img) :gray)]}
  (let [w (:width img)
        h (img/get-height img)
        result (into-array (map double-array (repeat w h)))]
    (reduce (fn [sum [x y]]
              (let [sum (if (= 0 y) 0 sum)
                    intensity (:gray (img/get-pixel img x y))
  ;                  _ (println x y)
 ;                   _ (println (map seq result))
                    neighbour-intens (if (<= x 0) 0 (aget result (dec x) y))
;                    _ (println neighbour-intens)
                    ]
                (aset-double result x y (+ intensity sum neighbour-intens))
                (+ sum intensity)))
            0
            (for [i (range 0 w)
                  j (range 0 h)]
              [i j]))
    result
    ))



(defn athreshold
  ([img]
     (threshold img 0.7))
  ([img threshold-value]
     (println threshold-value)

     (let [gray-img (imgc/to-grayscale img)
           w (:width img)
           h (img/get-height img)
           half-window-width (Math/round (double (/ w 8)))
           half-window-height (Math/round (double (/ h 8)))
           int-img (aintegral-img gray-img)
           prot-int-img (fn [[x y]] (if (and (<= 0 x (dec w))
                                           (<= 0 y (dec h)))
                                    (aget int-img x y)
                                    0))
           ]
       (Image.
        (doall (for [j (range h) i (range w)]
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
                 sum (- (prot-int-img [x2 y2] 0)
                        (prot-int-img [x2 (dec y1)] 0)
                        (prot-int-img [(dec x1) y2] 0)
                        (- (prot-int-img [(dec x1) (dec y1)] 0)))
                     _  (println sum)
                 ]
             (if (<= (* in  ct)
                     (* sum threshold-value))
               {:bw 0}
               {:bw 1}))))
        w))))

(defn my-for [init-i init-j]
  (loop [result nil i init-i j init-j]
    (println
      i j)
    (cond
     (= i j 0) (cons [i j] result)
     (= j 0) (recur (cons [i j] result) (dec i) init-j)
     :else (recur (cons [i j] result) i (dec j)))))
