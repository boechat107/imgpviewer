(ns image-processing.nfeatures
  (:require [image-processing.image-feature :as feat]
            [image-processing.image :as img]
            [image-processing.pixel :as pix]
)
  
  (:import [image_processing.image Image]))

;nfeatures is just a list of features:
;((...) (...) ... (...))
;and here lay all its functions

;there is also map-nfeat
;in which the features are mapped
;{1 (...) 2 (...) ... 255 (...)}

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x# "!\n") x#))

(defn feat-type? [feat]
  (let [super-sub-type [(type feat) (type (first feat))]]
    (cond
     (= super-sub-type [clojure.lang.PersistentArrayMap clojure.lang.MapEntry]) :map-nfeat
     (or (= super-sub-type [clojure.lang.PersistentList clojure.lang.PersistentList])
         (= super-sub-type [clojure.lang.LazySeq clojure.lang.PersistentList])
         (= super-sub-type [clojure.lang.PersistentList clojure.lang.LazySeq])
         (= super-sub-type [clojure.lang.LazySeq clojure.lang.LazySeq]))
     :nfeat
     (= super-sub-type [clojure.lang.PersistentList clojure.lang.PersistentList]) :nfeat
     (or (= super-sub-type [clojure.lang.PersistentList clojure.lang.PersistentArrayMap])
         (= super-sub-type [clojure.lang.LazySeq clojure.lang.PersistentArrayMap])) :feat
      :else (throw (Exception. "Feature type not identified!")))))

(defn cast-to-nfeat [feature]
  (let [feattype (feat-type? feature)]
    (cond
     (= feattype :nfeat) feature
     (= feattype :map-nfeat) (vals feature)
     (= feattype :feat) (list feature)
     :else (throw (Exception. "Unable to cast to nfeat")))))

;; (defmacro nfeatfn [name var & statements]
;;   `(def ~name (fn ~var
;;                 (let [~'nfeature (cast-to-nfeat ~'nfeature)]
;;                   ~@statements))))

(defn image-as-nfeat [Img]
  (list (feat/image-as-feature Img)))

(defn get-nfeature-max
  "get the nfeature max 'key'"
  [key nfeature]
  {:pre [(= (feat-type? nfeature) :nfeat)]}
  (apply max (map (partial feat/get-feature-max key) nfeature)))

(defn get-nfeature-min
  "get the nfeature min 'key'"
  [key nfeature]
  {:pre [(= (feat-type? nfeature) :nfeat)]}
  (apply min (map (partial feat/get-feature-min key) nfeature)))


(defn get-nfeature-height
  "Get the nfeature height, discounting white borders"
  [nfeature]
  {:pre [(= (feat-type? nfeature) :nfeat)]}
  (inc
   (- (get-nfeature-max :y nfeature) (get-nfeature-min :y nfeature))))

(defn get-nfeature-width
  "Get the nfeature width, discounting white borders"
  [nfeature]
  {:pre [(= (feat-type? nfeature) :nfeat)]}
  (inc
   (- (get-nfeature-max :x nfeature) (get-nfeature-min :x nfeature))))


(defn apply-nfeat-to-image
  "Given an 'Img', writes the 'nfeature' pixels into it"
  [Img nfeature]
  {:pre [(= (feat-type? nfeature) :nfeat)
         (= (class Img) image_processing.image.Image)]}
  (feat/apply-feature-to-image Img (reduce concat nfeature)))

(defn draw-nfeat-on-white-image
  [nfeature]
  {:pre [(= (feat-type? nfeature) :nfeat)]}
  (let [image-type (-> nfeature first first pix/pix-type)]
    (apply-nfeat-to-image (img/white-image image-type
                                           (->> nfeature (get-nfeature-max :x) inc)
                                           (->> nfeature (get-nfeature-max :y) inc))
                          nfeature)))

(defn split-into-connex
  "For a given nfeature, split each feature into its connex elements
   according to the (fn connect-fn [pix1 pix2])
   Returns a nfeature"
  [connect-fn nfeature]
  {:pre [(= (feat-type? nfeature) :nfeat)]}
  (mapcat (partial feat/split-feature-into-connex connect-fn) nfeature))


(defn nfilter
  "Applies filter all pixels in all filters, removes empty features"
  [function nfeature]
  {:pre [(= (feat-type? nfeature) :nfeat)]}
  (filter not-empty
          (map #(filter function %) nfeature)))

(defn nmap
  "Applies function in each pixel, in each feature"
  [function nfeature]
  {:pre [(= (feat-type? nfeature) :nfeat)]}
  (map #(map function %) nfeature))

(defn crop
  "crop the nfeature, if nfeature is in the center of the image,
reposition it the closes possible to the origin"
  [nfeature]
  {:pre [(= (feat-type? nfeature) :nfeat)]}
  (let [min-x (get-nfeature-min :x nfeature)
        min-y (get-nfeature-min :y nfeature)]
    (nmap #(assoc % :x (- (:x %) min-x)
                  :y (- (:y %) min-y)) nfeature)))

(defn paint-features-rnd-colors
  "View a feature"
;  [nfeature] (view-features nfeature nil)
  [nfeature]
  {:pre [(= (feat-type? nfeature) :nfeat)]}
  (let [rand-color (fn [] (reduce #(assoc %1 %2 (rand-int 150)) {:a 255} [:r :g :b]))]
    (map #(feat/paint-feature (rand-color) %1) nfeature)))

