(ns image-processing.test.hist-view-gui
  (:import
     (javax.imageio ImageIO) 
      (java.io File))
    (:use
     [clojure.test]
     [seesaw core make-widget border]
     [image-processing.hist-view-gui]))

; FIXME: wait for a user action.


(def x (map #(ImageIO/read (File. %)) 
            '("test/image_processing/test/1a7r.gif" 
              "test/image_processing/test/1azc.gif")))


(defn create-frame []
  (let [panel-info (create-hist-panel)
        hist-panel (first panel-info)
        img-group (second panel-info)]
    (add-imgs hist-panel img-group (first x) (second x))
    (-> (frame :title "Hello",
               :content  (scrollable hist-panel))
        ;:on-close :exit) 
        pack! 
        show!)
    [hist-panel img-group]))

#_(def z (create-frame))

#_(get-selected-img (second z))
