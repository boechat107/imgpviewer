(ns image-processing.nfeatures
  (:require [image-processing.image-feature :as feat]
            [image-processing.image :as img])
  (:import [image_processing.image Image]))

;nfeatures is just a list of features:
;([...] [...] ... [...])
;and here lay all its functions

;there is also map-nfeat
;in which the features are mapped
;{1 [...] 2 [...] ... 255 [...]}

(defn feat-type? [feat]
  (let [super-sub-type [(type feat) (type (first feat))]]
    (cond
     (= super-sub-type [clojure.lang.PersistentArrayMap clojure.lang.MapEntry]) :map-nfeat
     (= super-sub-type [clojure.lang.PersistentVector clojure.lang.PersistentVector]) :nfeat
     (= super-sub-type [clojure.lang.PersistentVector clojure.lang.PersistentArrayMap]) :feat
      :else (throw (Exception. "Feature type not identified!")))))

(defn cast-to-nfeat [feature]
  (let [feattype (feat-type? feature)]
    (cond
     (= feattype :nfeat) feature
     (= feattype :map-nfeat) (-> feature vals vec)
     (= feattype :feat) [feature]
     :else (throw (Exception. "Unable to cast to nfeat")))))

;; (defmacro nfeatfn [name var & statements]
;;   `(def ~name (fn ~var
;;                 (let [~'nfeature (cast-to-nfeat ~'nfeature)]
;;                   ~@statements))))

(defn image-as-nfeat [Img]
  (vector (feat/image-as-feature Img)))

(defn width [nfeature]
  "get the nfeature max :x +1!"
  {:pre [(= (feat-type? nfeature) :nfeat)]}
  (inc
   (apply max (map #(apply max (map :x %)) nfeature))))

;
(defn height [nfeature]
  "get the nfeature max :y +1!"
  {:pre [(= (feat-type? nfeature) :nfeat)]}
  (inc
   (apply max (map #(apply max (map :y %)) nfeature))))


(defn apply-nfeat-to-image [Img nfeature]
  "Given an 'Img', writes the 'nfeature' pixels into it"
  {:pre [(= (feat-type? nfeature) :nfeat)
         (= (class Img) image_processing.image.Image)]}
  (feat/apply-feature-to-image Img (reduce concat nfeature)))

(defn draw-nfeat-on-white-image [nfeature]
  {:pre [(= (feat-type? nfeature) :nfeat)]}
  (apply-nfeat-to-image (img/white-image :bw (width nfeature) (height nfeature)) nfeature))

(defn split-into-connex [connect-fn nfeature]
  "For a given nfeature, split each feature into its connex elements
   according to the (fn connect-fn [pix1 pix2])
   Returns a nfeature"
  {:pre [(= (feat-type? nfeature) :nfeat)]}
  (vec (mapcat (partial feat/split-feature-into-connex connect-fn) nfeature)))


(defn nfilter [function nfeature]
  "Applies filter all pixels in all filters, removes empty features"
  {:pre [(= (feat-type? nfeature) :nfeat)]}
  (vec (filter not-empty
               (map #(vec (filter function %)) nfeature))))

;
(defn nmap [function nfeature]
  "Applies function in each pixel, in each feature"
  {:pre [(= (feat-type? nfeature) :nfeat)]}
  (vec (map #(vec (map function %)) nfeature)))

;


(defn crop [nfeature]
  "crop the nfeature, if nfeature is in the center of the image,
reposition it the closes possible to the origin"
  {:pre [(= (feat-type? nfeature) :nfeat)]}
  (let [min-x (apply min (map #(apply min (map :x %)) nfeature))
        min-y (apply min (map #(apply min (map :y %)) nfeature))]
    (nmap #(assoc % :x (- (:x %) min-x)
                  :y (- (:y %) min-y)) nfeature)))



