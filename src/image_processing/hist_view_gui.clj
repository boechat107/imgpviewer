(ns image-processing.hist-view-gui
    (:import 
      (javax.imageio ImageIO) 
      (java.io File)
      (java.awt.image BufferedImage)) 
    (:use 
      [clojure.java.io] 
      [seesaw core make-widget dev])) ;; FIXME: remove dev 

;; TODO: listen the resize event. change the number of columns according to size.

;; TODO: selected-image: returns the matrix of points instead of the BufferedImage

;; TODO: add-image: second version that receives a matrix of points.

(defn create-hist-panel
  "Creates and returns a grid-panel and a button-group. The default number
      of columns is 3."
      #^{:arglists [n-columns]}
      ([] (create-hist-panel 3)) 
      ([n-columns]
       (let [img-group (button-group)
             hist-panel (grid-panel
                          :border 5
                          :hgap 10 :vgap 10
                          :columns n-columns)]
         [hist-panel img-group])))


(defn create-radio-img
  "Creates a horizontal-panel with a radio button and a BufferedImage."
  #^{:arglists [buff-img img-group]}
  [buff-img img-group]
  (horizontal-panel
    ;; The image's path can be retrieved by the user-data of the radio
    ;; button.
    :items [(radio :group img-group :user-data buff-img) (label :icon buff-img)]))


(defn add-imgs
  "Add images to hist-panel."
  #^{:arglists [hist-panel img-group & buff-imgs]}
  [hist-panel img-group & buff-imgs]
  (let [previous-imgs (vec (config hist-panel :items))]
    (config! hist-panel :items (reduce #(conj %1 (create-radio-img %2 img-group))
                                       previous-imgs
                                       buff-imgs))))


(defn get-selected-img
  "Returns the BufferedImage selected at the history viewer. If no image was selected, it
      returns null."
  #^{:arglists [img-group]}
  [img-group]
  (let [selected-radio (selection img-group)]
    (if (not= nil selected-radio) (config selected-radio :user-data))))

