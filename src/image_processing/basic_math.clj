(ns image-processing.basic-math
  (:require [clojure.contrib.math :as cmath])
  (:import
      ;[org.apache.commons.math.stat.descriptive.moment Variance]
      [java.lang Math]))

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x# "!\n") x#))

(defn square
  "Returns the square of X."
  #^{:arglists [x]}
  [x]
  (* x x))

(defn mean
  "Returns the mean value of a sequence."
  [coll]
  (/ (reduce + coll) (count coll)))

(defn stdev
  "Returns the standard deviation value of a sequence."
  [coll]
  #^{:arglists [coll]}
  (let [mean (mean coll)]
    (Math/sqrt (double (/ (reduce + (map #(Math/pow (- %1 mean) 2) coll)) (count coll))))))

(defn split-by-symetrical-operator
  "Split a vector elements into equivalent partitions, according to the symetrical two argument function related?
   (fn related? [elem1 elem2]): must return true if elem1 and elem2 are related, false otherwise"
  ([related? data]
     {:pre [(not-empty data)
            (fn? related?)]
      :post [(not-empty %)]}
     (let [get-related-to-elem
           (fn [start-elems data]
             (loop [related nil
                    to-check-elems start-elems
                    remaining-elems data]
               (if-let [checking-elem (first to-check-elems)]
                 (let [rest-to-check (rest to-check-elems)
                       [related-to-elem non-related-to-elem] (let [tmp (group-by (partial related? checking-elem) remaining-elems)]
                                                               [(tmp true) (tmp false)])]
                   (recur (conj related checking-elem) (concat rest-to-check related-to-elem) non-related-to-elem))
                 [related remaining-elems])))]
       (loop [result nil data data]
         (if (not-empty data)
           (let [[related-elems remaining-elems]
                 (get-related-to-elem [(first data)] (rest data))]
             (recur (conj result related-elems)
                    remaining-elems))
           result)))))

