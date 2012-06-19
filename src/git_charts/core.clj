(ns git-charts.core
  (:use [incanter.core] [incanter.zoo])
  (:require (incanter stats charts io)))

(defn read-dataset []
  (incanter.io/read-dataset "/Users/jen/Dropbox/legacy codebase/ove-gitstats/lines_of_code.dat" :delim \space))

(defn clean-dataset [ds]
  (incanter.core/col-names ds [:Date :Locs]))

(defn fix-dates [ds]
  (conj-cols (map #(* 1000 %) (sel ds :cols :Date)) (sel ds :cols :Locs) ))

(defn downsample [window-size ds]
  (let [downsampled (roll-mean window-size (sel ds :cols :Locs))
        rows-to-pick (take-while #(< % (count downsampled)) (iterate #(+ window-size %) 0))
        ]
    (sel 
     (conj-cols (sel ds :cols :Date) downsampled) :rows rows-to-pick ) )) 

(defn create-chart [ds]  (incanter.charts/time-series-plot :Date :Locs :x-label "Date"  :y-label "Lines of code" :data ds))


(defn doitall [window-size]
  (->> (read-dataset)
      (clean-dataset)
      (downsample window-size)
      (clean-dataset) ;; ooops
      (fix-dates)
      (clean-dataset) ;; double oops
      (create-chart)
      (view)))