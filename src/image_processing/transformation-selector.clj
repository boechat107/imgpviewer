(ns image-processing.transformation-selector
  [:use
   [seesaw.core]
   [clojure.repl]])

(defn transformations-selector [functions]
  (vertical-panel
   :items
   [(scrollable (listbox :model (map #(let [fn-meta (meta (resolve %1))]
                                        (format "name :%s"
                                                (:name fn-meta)))
                                     functions)))
    (button :text "Next"
            :mnemonic \N
            :listen [:action (fn [_] (alert "NEXT!"))])]))

(defn see-component [comp]
  (invoke-now (-> (frame :title "Check out" :content comp) pack! show!)))










