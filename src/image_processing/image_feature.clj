(ns image-processing.image-feature
  (:require [image-processing.image]
            [image-processing.pixel :as pix]
            [image-processing.basic-math :as bmath])

  (:import [image_processing.image Image]))
;;an image feature is just a list of pixels
;;(pix-1 pix-2 ... pix-n)

;;this class is serves only to define functions for
;;image-feature, they are not closed operations

;;upper abstraction, n-feat is closed =)

(defn image-as-feature
  "Transform an Image to an image-feature"
  [Img]
  (let [pixels (:pixels Img)
        width (:width Img)]
    (map #(assoc %1 :x (first %2) :y (second %2))
         pixels
         (for [y (range (/ (count pixels) width)), x (range width)] [x y]))))

(defn apply-feature-to-image [Img feature]
  (Image.
   (reduce #(assoc %1 (+ (:x %2) (* (:width Img) (:y %2))) (dissoc %2 :x :y)) (:pixels Img) feature)
   (:width Img)))


(defn get-feature-max
  "Get the feature max fn value"
  [key feature]
  (apply max (map key feature)))

(defn get-feature-min
  "Get the feature min fn value"
  [key feature]
  (apply min (map key feature)))

(defn get-feature-width
  "returns the width of the rectangle that fits the feature"
  [feature]
  (inc (- (get-feature-max :x feature) (get-feature-min :x feature))))

(defn get-feature-height
  "returns the height of the rectangle that fits the feature"
  [feature]
  (inc
   (- (get-feature-max :y feature) (get-feature-min :y feature))))


(defn crop-feature
  "If the feature is in the middle of the image, it will remove empty spaces from the borders "
  [feature]
  (let [min-width (apply min (map :x feature))
        min-height (apply min (map :y feature))]
    (map #(assoc % :x (- (:x %) min-width)
                   :y (- (:y %) min-height))
         feature)))

(defn split-feature-into-connex
  "Split a 'feature' into its connex elements, according to
   'connected?-fn.
   (defn connected?-fn [pixel1 pixel2]
       returns true if 'pixel1' and 'pixel2' are connex,
       false otherwise)"
  ([connected?-fn feature]
     (bmath/split-by-symetrical-operator connected?-fn feature)))

(defn paint-feature [color feature]
  {:pre [(not= nil (pix/pix-type color))]}
  (map #(into % color) feature))
