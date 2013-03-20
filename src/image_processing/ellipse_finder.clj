(ns image-processing.ellipse-finder
  (:require [image-processing.point :as p]
            [clojure.contrib.math :as math]))

#_(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x# "!\n") x#))

(defn- ellipse-b
  "Given two points a1 and a2 that determine one elipse axis
   and a vector, returns the corresponding length for the
   other axis"
  [a1 a2 v]
  (let [m (p/mid-point a1 a2)
        a2a1 (p/subtract a2 a1)
        v-polar (p/subtract v m)
        lenR (p/length v-polar)
        lenA (/ (p/length a2a1) 2)
        theta (p/angle a2a1 v-polar)]
    (/ (* lenR lenA (math/abs (Math/sin theta)))
       (math/sqrt
        (- (math/expt lenA 2)
           (math/expt (* lenR  (Math/cos theta)) 2))))))

(defn- protected-ellipse-b
  "Protects the calculation for the ellipse b,
   for all sort of results are possible depending
   on the points configuration: division by zero, sqrt(-1).

   So if its an error, returns nil"
  [a1 a2 x]
  (try
    (let [b (ellipse-b a1 a2 x)]
      (if (Double/isNaN b)
        nil
        (math/round b)))
    (catch Exception e nil)))

(defn- get-triplets-index
  "Returns a triplet of index within 1..n that will be used to
   get the points from the feature to calculate the best fitting
   ellipse.
   It returns [i_a1=0..n-1 i_a2=i+1..n-1 i_x=0..n-1
   where i_a1<>i_x and i_a2<>i_x.
   In other words, this function will return all the index combinations
   of points that have meaning, when calculating the ellipse:
       -they are not the same,
       -the major axis a2a1 and a1a2 are not checked twice
        (they are the same ellipse, daaaaaah!)"
  [n]
  (let [interval (range 0 n)]
    (filter #(let [[i_a1 i_a2 i_x] %] (and (< i_a1 i_a2) (not= i_a1 i_x) (not= i_a2 i_x)))
            (for [i_a1 interval i_a2 interval i_x interval]
              (vector i_a1 i_a2 i_x)))))

(defn- get-triplets
  "This functions get the triplet index and get the point triplets from
   feature"
  [feat]
  (map #(map (partial get feat) %) (get-triplets-index (count feat))))

(defn- in
  "Simple function to check if an 'x' is within an [a b] closed interval"
  [x ab]
  (<= (first ab) x (second ab)))

(defn find-ellipse
  "Find the ellipse that passes by most pixels in 'feature'.
   The ellipse major axis and minor axis length must be between
   Ainterval and Binterval, respectively.

   Interval is a pair [min max] value.

   Returns:
   [pixel_count point_a1 point_a2 b_value]
      pixel_count - pixels on ellipse trajectory
      point_a1, point_a2 - points that establish the major axis
      b_value - minor axis length"
  [feature Ainterval Binterval]
  (let [result {:counter {} :best [0 nil nil 0]}
        inc-result
        (fn [result a1 a2 b]
          (let [counter (result :counter)
                updated-count (inc (get counter [a1 a2 b] 0))
                updated-counter (assoc counter [a1 a2 b] updated-count)
                best-count (first (result :best))]
            (if (> updated-count best-count)
              {:counter updated-counter :best [updated-count a1 a2 b]}
              (assoc result :counter updated-counter))))
        process-triplet (fn [result triplet]
                     (let [[a1 a2 x] triplet]
                       (if (in (p/length (p/subtract a2 a1)) Ainterval)
                         (if-let [b (protected-ellipse-b a1 a2 x)]
                           (if (in b Binterval)
                             (inc-result result a1 a2 b)
                             result)
                           result)
                         result)))]
    (:best (reduce process-triplet result (get-triplets feature)))))

(defn- is-inside-ellipse? [a1 a2 b x]
  (if-let [calculated_b (protected-ellipse-b a1 a2 x)]
    (< calculated_b b)))

(defn pixels-inside-ellipse [a1 a2 b feat]
  (filter #(is-inside-ellipse? a1 a2 b %) feat))
