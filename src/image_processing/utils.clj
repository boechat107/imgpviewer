(ns image-processing.utils
  "Utilities."
  (:use [clojure.reflect]
        [clojure.set]
        [clojure.pprint]))

(defn print-methods-table [obj]
  "pprint all methods, parameter, and return types of a object,
sorted by method name."
  (let [columns [:name :parameter-types :return-type]
        reflection (reflect obj)]
    (print-table columns
                 (sort-by :name (project (:members reflection) columns)))
    ;; (println (format "Bases --> %s%nAbove table for class --> %s"
    ;;                  (:bases reflection) obj))
    ))

(defn mult-vec
  "Applies a element-by-element multiplication among vectors. 
  EX.: (mult-vec [1 2 3] 2)
       (mult-vec 2 [2 3 4] [2 2 2] 1)"
  [v & vs]
  (let [step (fn [v a]
               (cond 
                 (and (sequential? v) (sequential? a))
                 (map * v a)
                 ;
                 (sequential? v)
                 (map #(* a %) v)
                 ;
                 (sequential? a)
                 (map #(* v %) a)
                 ;
                 :else (* v a)))]
    (if (seq vs)
      (recur (step v (first vs)) (rest vs))
      v)))

(defmacro mult-aget
  "Returns the value of an element of multiple dimensional arrays. Uses type hints to 
  improve the performance of aget.
  Reference:
  http://clj-me.cgrand.net/2009/10/15/multidim-arrays/"
  ([hint array idx]
   `(aget ~(vary-meta array assoc :tag hint) ~idx))
  ([hint array idx & idxs]
   `(let [a# (aget ~(vary-meta array assoc :tag 'objects) ~idx)]
      (mult-aget ~hint a# ~@idxs))))

(defmacro mult-aset
  "Sets the value of an element of a multiple dimensional array. Uses type hints to 
  improve the performance of aset. (Only for double and int arrays for now)
  Reference:
  http://clj-me.cgrand.net/2009/10/15/multidim-arrays/"
  [hint array & idxsv]
  (let [hints '{doubles double ints int}
        [v idx & sxdi] (reverse idxsv)
        idxs (reverse sxdi)
        v (if-let [h (hints hint)] (list h v) v)
        nested-array (if (seq idxs)
                       `(mult-aget ~'objects ~array ~@idxs)
                       array)
        a-sym (with-meta (gensym "a") {:tag hint})]
    `(let [~a-sym ~nested-array]
       (aset ~a-sym ~idx ~v))))

(defmacro mult-aclone
  "Returns a clone of a two dimensional array."
  [hint array]
  `(let [array# ~(vary-meta array assoc :tag 'objects)
         m# (aclone array#)]
     (dotimes [n# (alength array#)]
       (let [a# (aget array# n#)]
         (aset m# n# (aclone a#))))
     m#))
