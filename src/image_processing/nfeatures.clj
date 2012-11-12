(ns image-processing.nfeatures
  (:require [image-processing.image-feature :as feat]
            [image-processing.image :as img]
            [image-processing.pixel :as pix])
  (:import [image_processing.image Image]))

;nfeatures is just a list of features:
;((...) (...) ... (...))
;and here lay all its functions

#_(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x# "!\n") x#))

(defn nfeat? [feat]
  (try
    (let [super-sub-type [(type feat) (type (first feat))]]
      (if (or (= super-sub-type [clojure.lang.PersistentList clojure.lang.PersistentList])
              (= super-sub-type [clojure.lang.LazySeq clojure.lang.PersistentList])
              (= super-sub-type [clojure.lang.PersistentList clojure.lang.LazySeq])
              (= super-sub-type [clojure.lang.LazySeq clojure.lang.LazySeq]))
        true
        false))
    (catch Exception E
      false)))

(defn image-as-nfeat [Img]
  (list (feat/image-as-feature Img)))

(defn get-nfeature-max
  "get the the max value for a 'key' among all nfeatures"
  [key nfeature]
  {:pre [(nfeat? nfeature)]}
  (apply max (map (partial feat/get-feature-max key) nfeature)))

(defn get-nfeature-min
  "get the the min value for a 'key' among all nfeatures"
  [key nfeature]
  {:pre [(nfeat? nfeature)]}
  (apply min (map (partial feat/get-feature-min key) nfeature)))


(defn get-nfeature-height
  "Get the nfeature height, discounting white borders"
  [nfeature]
  {:pre [(nfeat? nfeature)]}
  (inc
   (- (get-nfeature-max :y nfeature) (get-nfeature-min :y nfeature))))

(defn get-nfeature-width
  "Get the nfeature width, discounting white borders"
  [nfeature]
  {:pre [(nfeat? nfeature)]}
  (inc
   (- (get-nfeature-max :x nfeature) (get-nfeature-min :x nfeature))))


(defn apply-nfeat-to-image
  "Given an 'Img', writes the 'nfeature' pixels into it"
  [Img nfeature]
  {:pre [(nfeat? nfeature)
         (= (class Img) image_processing.image.Image)]}
  (feat/apply-feature-to-image Img (reduce concat nfeature)))

(defn draw-nfeat-on-white-image
  [nfeature]
  {:pre [(nfeat? nfeature)]}
  (let [image-type (-> nfeature first first pix/pix-type)]
    (apply-nfeat-to-image (img/white-image image-type
                                           (->> nfeature (get-nfeature-max :x) inc)
                                           (->> nfeature (get-nfeature-max :y) inc))
                          nfeature)))

(defn draw-nfeat-on-blank-image
  [nfeature]
  {:pre [(nfeat? nfeature)]}
  (let [image-type (-> nfeature first first pix/pix-type)]
    (apply-nfeat-to-image (img/blank-image
                                           (->> nfeature (get-nfeature-max :x) inc)
                                           (->> nfeature (get-nfeature-max :y) inc))
                          nfeature)))

(defn split-into-connex
  "For a given nfeature, split each feature into its connex elements
   according to the (fn connect-fn [pix1 pix2])
   Returns a nfeature"
  [connect-fn nfeature]
  {:pre [(nfeat? nfeature)]}
  (mapcat (partial feat/split-feature-into-connex connect-fn) nfeature))

(defn nfilter
  "filter pixels from all nfeatures, removes empty features"
  [function nfeature]
  {:pre [(nfeat? nfeature)]}
  (filter not-empty
          (map #(filter function %) nfeature)))

(defn nmap
  "Applies function to all pixels, in all features"
  [function nfeature]
  {:pre [(nfeat? nfeature)]}
  (map #(map function %) nfeature))

(defn crop
  "crop the nfeature
   if nfeature is in the center of the image,
reposition it the closes possible to the origin"
  [nfeature]
  {:pre [(nfeat? nfeature)]}
  (let [min-x (get-nfeature-min :x nfeature)
        min-y (get-nfeature-min :y nfeature)]
    (nmap #(assoc % :x (- (:x %) min-x)
                  :y (- (:y %) min-y)) nfeature)))

(defn paint-features-rnd-colors
  "Paint each feature from a nfeature with a random color.
   Usually used for debug =)"
  [nfeature]
  {:pre [(nfeat? nfeature)]}
  (let [rand-color (fn [] (reduce #(assoc %1 %2 (+ 25 (rand-int 150))) {:a 255} [:r :g :b]))]
    (map #(feat/paint-feature (rand-color) %1) nfeature)))
