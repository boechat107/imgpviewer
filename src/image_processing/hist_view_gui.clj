(ns image-processing.hist-view-gui
    (:import 
      (javax.imageio ImageIO) 
      (java.io File)) 
    (:use 
      [clojure.java.io] 
      [seesaw core make-widget border]))


(defn create-hist-panel [img-paths]
  "Creates a horizontal panel showing the images placed at the input paths."
  #^ {:arglists [img-paths]}
  (let [imgs-url (map #(as-url (File. %)) img-paths)]
    (horizontal-panel :items (vec imgs-url))))



;(def x '("test/image_processing/test/1a7r.gif"
;         "test/image_processing/test/1azc.gif"))

;(defn create-frame [] 
;  (invoke-later (-> (frame :title "Hello",
;             :content  (create-hist-panel x) ,
;             :on-close :exit)
;      pack!
;      show!))) 

