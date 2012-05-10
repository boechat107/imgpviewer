(ns image-processing.selector
  [:use [seesaw.core]])

(defn get-elements-from
  "Gets all the elements from a particular listbox."
  [a-listbox]
  (try 
    (-> a-listbox .getModel .toArray vec)
    (catch java.lang.IllegalArgumentException e [])))

(defn difference
  "takes the diff between 2 seqs"
  [seq1 seq2]
  (filter #(= (.indexOf seq2 %) -1) seq1))

(defn transfer-itens
  "Transfer every item selected from one listbox to another."
  [source-listbox destination-listbox]
    (if-let [selected-itens (selection source-listbox {:multi? true})]
      (do
        (config! source-listbox :model 
                 (difference (get-elements-from source-listbox) 
                             selected-itens))
        (config! destination-listbox :model 
                 (concat (get-elements-from destination-listbox)
                         selected-itens)))))

(defn selector
  "Return the selector. The objects-map should be a map of the form:

name-string . (object-value object-description)

The list of selected values will be passed to action when the user
presses OK. "
  [objects-map action]  
  (let [min-size [200 :by 100]
        names
        (into (sorted-set) (keys objects-map))
        
        available-listbox
        (listbox :model names
                 :tip "Available options. Please use the buttons to the right to select one of them."
                 :size min-size)
        selection-listbox
        (listbox :tip "Selection. Please use the buttons to the left to deselect one of them."
                 :size min-size)
        
        select-button
        (button :text "→"
                :listen [:action (fn [_]
                                   (transfer-itens available-listbox
                                                   selection-listbox))])
        remove-button
        (button :text "←"
                :listen [:action (fn [_] (transfer-itens selection-listbox
                                                         available-listbox))])
        accept-button
        (button :text "Ok"
                :mnemonic \O
                :listen
                [:action (fn [_]
                           (println (get-elements-from selection-listbox))
                           (action (map #(first (get objects-map %1))
                                        (get-elements-from selection-listbox))))])]    
    (vertical-panel :items
                    [(horizontal-panel 
                      :items
                      [(scrollable available-listbox)
                       (vertical-panel :items [select-button remove-button])
                       (scrollable selection-listbox)])
                     (border-panel :east accept-button)])))

(defn see-component [comp]
  (invoke-now (-> (frame :title "Check out" :content comp) pack! show!)))

;; (reduce #(let [resolved-symbol (resolve %2)]
;;                                      (merge %1 {(:name (meta resolved-symbol))
;;                                                 resolved-symbol}))
;;                                   {}
;;                                   functions)
