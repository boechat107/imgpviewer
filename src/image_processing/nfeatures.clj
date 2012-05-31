(ns image-processing.nfeatures
  (:require [image-processing.image-feature :as feat]))

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

(defmacro nfeatfn [name var & statements]
  `(def ~name (fn ~var
                (let [~'nfeature (cast-to-nfeat ~'nfeature)]
                  ~@statements))))

(nfeatfn split-into-connex [connect-fn nfeature]
         (vec  (mapcat (partial feat/split-feature-into-connex connect-fn) nfeature)))

;as we have different pixels, i think we should  unify this pixel
;operation to handle all types of pixel, so that i dont have to 
;worry about it 
;
;; (nfeatfn map-to-gray [nfeature]
;;          (let [joint-nfeat (reduce concat nfeature)]
;;            (group-by #(-> (:color %) pix/avgRgbToIntensity Math/ceil) joint-feat)))

















