(ns image-processing.charts
    (:import
      [java.awt Color]
      [org.jfree.chart  ChartFactory
                        ChartUtilities
                        ChartFrame
                        ChartTheme
                        StandardChartTheme]
      [org.jfree.data.statistics    HistogramDataset
                                    HistogramType
                                    DefaultBoxAndWhiskerCategoryDataset]
      [org.jfree.data.xy XYSeries XYSeriesCollection])
    (:use
     [image-processing.core :only (convert-image-to-buffImg)]
     [image-processing.nfeatures :as nfeat]
     [seesaw core make-widget]))


; TODO: option to rotate the histogram for horizontal histograms.
(defn histogram-chart
  "Generates a JFree histogram object for a collection X of frequencies.
   OPTS is a map of options:
   :title
   :x-label
   :y-label
   :series-label
   :legend?

   EXAMPLE:
   (view (histogram-chart [1 5 3 10 7])
   
   References:
   http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html
   https://github.com/liebke/incanter/blob/master/modules/incanter-charts/src/incanter/charts.clj "
  #^{:arglists [[x] [x opts]]}
  ([x] (histogram-chart x nil))
  ([x opts]
   (let [title (or (:title opts) "Histogram")
         x-lab (or (:x-label opts) (str 'x))
         y-lab (or (:y-label opts) "Frequency")
         series-lab (or (:series-label opts) (str 'x))
         legend? (true? (:legend opts))
         background-img (or (:bg-img opts) nil)
         data-series (XYSeries. series-lab)
         dataset (XYSeriesCollection.)]
     (dorun (map #(.add data-series %1 %2) (range (count x)) x)) 
     (.addSeries dataset data-series)
     (let [chart (org.jfree.chart.ChartFactory/createHistogram
                   title
                   x-lab
                   y-lab
                   dataset
                   org.jfree.chart.plot.PlotOrientation/VERTICAL
                   legend?		; no legend
                   true			; tooltips
                   false)] 
       ;(.setBackgroundImage chart background-img) ; does not work
       ;(.setBackgroundPaint chart (Color. 0 0 0 0)) ; does not work
       chart))))


   (defmulti view
             (fn [arg & more]
                 (type arg)))

(defmethod view org.jfree.chart.JFreeChart
  [chart & more]
  (let [window-title "JFree Chart"
        width 500
        height 400
        frame (ChartFrame. window-title chart)]
    (doto frame
          (.setSize width height)
          (.setVisible true))
    frame))

(defmethod view java.awt.image.BufferedImage
  [buff-img & more]
  (let [grid (grid-panel
               :border 5
               :hgap 10 :vgap 10
               :columns (min 6 (max 1 (count more))) 
               :items (map #(label :icon %) (conj more buff-img)))]
    (-> (frame :title "Image Viewer" 
               :content grid)
        pack!
        show!)))

(defmethod view image_processing.image.Image
  [img & more]
  (-> (frame :title "Image Viewer" 
             :content (label :border 10 :icon (convert-image-to-buffImg img)))
      pack!
      show!)) 

(defn view-nfeature [nfeat]
;  (println nfeat)
  (-> nfeat nfeat/paint-features-rnd-colors
      nfeat/draw-nfeat-on-white-image
      view)
  nfeat
  )