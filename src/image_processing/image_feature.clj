(ns image-processing.image-feature
  (:require [image-processing.image])
  (:import [image_processing.image Image]))
;an image feature is just a vector of pixels
;[pix-1 pix-2 ... pix-n]

;this class is serves only to define functions for
;image-feature, they are not closed operations

;upper abstraction, n-feat is closed =)

(defn image-as-feature [Img]
  "Transform an Image to an image-feature"
  (let [pixels (:pixels Img)
        width (:width Img)]
    (vec
     (map #(assoc %1 :x (first %2) :y (second %2))
          pixels
          (for [y (range (/ (count pixels) width)), x (range width)] [x y])))))

(defn apply-feature-to-image [Img feature]
  (Image.
   (reduce #(assoc %1 (+ (:x %2) (* (:width Img) (:y %2))) (dissoc %2 :x :y)) (:pixels Img) feature)
   (:width Img)))



(defn feature-width [feature]
  "Get the feature width, max x value"
  #^{:arglist [feature]}
  (inc (reduce #(max %1 (:x %2)) 0 feature)))

(defn feature-height [feature]
  "Get the feature height, max y value"
  #^{:arglist [feature]}
  (inc (reduce #(max %1 (:y %2)) 0 feature)))


(defn crop-feature [feature]
  "If the feature is in the middle of the image, it will remove empty spaces from the borders "
  (let [min-width (apply min (map :x feature))
        min-height (apply min (map :y feature))]
    (map #(assoc % :x (- (:x %) min-width)
                   :y (- (:y %) min-height))
         feature)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Creation of the get connex features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- connex-pixs-for-pix
  "For a vector 'start-pixels', find the pixels in 'feature' that are
   connex, according to the 'connected?-fn' function."
  #^{:arglist [start-pixels feature connected?-fn]}
  ([connected?-fn start-pixels feature]
     {:pre [(vector? feature)
            (vector? start-pixels)
            (not-empty start-pixels)]
      :post [(not-empty (first %))
             (vector? (first %))]}
     
     (loop [connex []
            to-check-pixs start-pixels
            remaining-pixels feature]  
       (if-let [pix (first to-check-pixs)]
         (let [rest-to-check (rest to-check-pixs)
               [neighbours-for-pix other-pix] (let [tmp (group-by (partial connected?-fn pix) remaining-pixels)]
                                                [(tmp true) (tmp false)])]
           (recur (conj connex pix) (concat rest-to-check neighbours-for-pix) other-pix))
         [connex remaining-pixels]))))

(defn split-feature-into-connex
  "Split a 'feature' into its connex elements, according to
   'connected?-fn.
   (defn connected?-fn [pixel1 pixel2]
       returns true if 'pixel1' and 'pixel2' are connex,
       false otherwise)"
  #^{:arglist [feature connected?-fn]}
  ([connected?-fn feature]
     {:pre [(vector? feature)
            (not-empty feature)
            (fn? connected?-fn)]
      :post [(not-empty %)]}
     (loop [result [] feature feature]
       (if (not-empty feature)
         (let [[connex-pixels remaining-pixels]
               (connex-pixs-for-pix connected?-fn [(first feature)] (-> feature rest vec))]
           (recur (conj result connex-pixels)
                  remaining-pixels))
         result))))
