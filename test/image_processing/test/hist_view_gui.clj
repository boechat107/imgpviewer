(ns image-processing.test.hist-view-gui
    (:use
      [clojure.test]
      [seesaw core make-widget border]
      [image-processing.hist-view-gui]))

; FIXME: wait for a user action.


(def x '("test/image_processing/test/1a7r.gif"
         "test/image_processing/test/1azc.gif"))


(deftest create-frame
  (invoke-now (-> (frame :title "Hello",
             :content  (create-hist-panel x) ,
             :on-close :exit)
      pack!
      show!)))
