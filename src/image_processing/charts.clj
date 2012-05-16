(ns image-processing.charts
    (:import
      [org.jfree.chart ChartFactory ChartUtilities ChartFrame ChartTheme StandardChartTheme]
      [org.jfree.data.statistics    HistogramDataset
                                    HistogramType
                                    DefaultBoxAndWhiskerCategoryDataset])
    (:use
      [seesaw core make-widget]))


;(defn histogram*
;  ([x & options]
;    (let [opts (if options (apply assoc {} options) {})
;          data (:data opts)
;          _x (if (coll? x) (to-list x) ($ x data))
;          nbins (or (:nbins opts) 10)
;          theme (or (:theme opts) :default)
;          density? (true? (:density opts))
;          title (or (:title opts) "")
;          x-lab (or (:x-label opts) (str 'x))
;          y-lab (or (:y-label opts)
;                     (if density? "Density" "Frequency"))
;          series-lab (or (:series-label opts) (str 'x))
;          legend? (true? (:legend opts))
;          dataset (HistogramDataset.)]
;      (do
;        (.addSeries dataset series-lab (double-array _x) nbins)
;        (when density? (.setType dataset org.jfree.data.statistics.HistogramType/SCALE_AREA_TO_1))
;        (let [chart (-> (org.jfree.chart.ChartFactory/createHistogram
;                          title
;                          x-lab
;                          y-lab
;                          dataset
;                          org.jfree.chart.plot.PlotOrientation/VERTICAL
;                          legend?		; no legend
;                          true			; tooltips
;                          false)
;                        (set-theme theme))]
;          chart)))))


(defmulti view (fn [arg] (type arg)))

(defmethod view org.jfree.chart.JFreeChart
  ([chart & options]
    (let [opts (when options (apply assoc {} options))
          window-title (or (:window-title opts) "Incanter Plot")
          width (or (:width opts) 500)
          height (or (:height opts) 400)
          frame (ChartFrame. window-title chart)]
      (doto frame
        (.setSize width height)
        (.setVisible true))
      frame)))

(defmethod view java.awt.image.BufferedImage
  [buff-img]
  (-> (frame :title "Image Viewer" 
             :content (label :border 10 :icon buff-img))
      pack!
      show!))
