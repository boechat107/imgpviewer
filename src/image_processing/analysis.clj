(ns image-processing.analysis
  (:require 
    [image-processing.core-new :as ipc] 
    [incanter.core :as ic]
    )
  )

(defn conc-row-histo
  "Returns a list of the concentration of a color in each row of an Image."
  ([img]
   {:pre [(ipc/gray-type? img)]}
   (->> (:channels img)
        first
        (map ic/sum))))

(defn conc-column-histo
  "Returns a list of the concentration of a color in each column of an Image."
  ([img]
   (->> (:channels img)
        first
        ic/transpose
        vector
        (assoc img :channels)
        conc-row-histo)))
