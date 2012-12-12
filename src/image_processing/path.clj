(ns image-processing.path
  (:require [image-processing
             [image-feature :as feat]
             [nfeatures :as nfeat]
             [image :as img]
             [pixel :as pix]]))


;;a path is a seq of points which cuts an image 
;;  vertically - vertical-path 
;;  horizontally - horizontal-path
;; note that a path is an image-feature, but no the converse ;)

(defn v-path
  "Given an 'x' position and the image 'height', generates a vertical path"
  [x height]
  (map #(hash-map :x x :y %) (range height)))

(defn h-path
  "Given an 'y' position and the image 'width', generates a horizontal path"
  [y width]
  (map #(hash-map :x % :y y) (range width)))

(defn is-v-path?
  "Verifies if it's a v-path, i.e. it has one point for each vertical y-coordinate"
  [path]
  (every? true?
         (map #(= 1 (Math/abs (- (:y %1) (:y %2))))
              path
              (rest path))))

(defn is-h-path?
  "Verifies if it's a v-path, i.e. it has one point for each vertical y-coordinate"
  [path]
  (every? true?
         (map #(= 1 (Math/abs (- (:x %1) (:x %2))))
              path
              (rest path))))

(defn v-path-fits-image?
  [path img]
  {:pre [(is-v-path? path)]}
  (and (= (feat/get-feature-max :y path)
          (dec (img/get-height img)))
       (= (feat/get-feature-min :y path)
          0)
       (<= 0
           (feat/get-feature-min :x path)
           (feat/get-feature-max :x path)
           (dec (:width img)))))

(defn h-path-fits-image?
  [path img]
  {:pre [(is-h-path? path)]}
  (and (= (feat/get-feature-max :x path)
          (dec (:width img)))
       (= (feat/get-feature-min :x path)
          0)
       (<= 0
           (feat/get-feature-min :y path)
           (feat/get-feature-max :y path)
           (dec (img/get-height img)))))

(defn paint-path
  ([path img]
     (paint-path path img (pix/RED :argb)))
  ([path img color]
     {:pre [(= (img/get-image-type img)
               (pix/pix-type color))]}
     (feat/apply-feature-to-image img
                                  (feat/paint-feature color path))))

(def paint-path-rnd-colors nfeat/paint-features-rnd-colors)

(defn feature-between-vertical-paths
  "Get image between two vertical paths, even if the path cross eachother"
  [pathA pathB img]
  {:pre [(v-path-fits-image? pathA img)
         (v-path-fits-image? pathB img)]}
  (let [pathA (if (= (-> pathA first :y)
                     (-> pathB first :y))
                pathA
                (reverse pathA))]

    (for [[point1 point2] (map list pathA pathB)
          x (range 0 (:width img))
          :let [[from_x to_x] ((juxt min max) (:x point1) (:x point2))]
          :when (<= from_x x to_x)]
      (do
        (let [y (do (assert (= (:y point1) (:y point2)))
                    (:y point1))]
          (into {:x x :y y} (img/get-pixel img x y)))))))

(defn nfeature-between-vertical-paths
  [pathA pathB img]
  (list (feature-between-vertical-paths pathA pathB img)))

(defn image-between-vertical-paths [pathA pathB img]
  (nfeat/draw-nfeat-on-white-image
   (nfeat/crop
    (nfeature-between-vertical-paths pathA pathB img))))