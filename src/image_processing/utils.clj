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
