(ns git-charts.core
  (:use [incanter.core] [incanter.zoo])
  (:require (incanter stats charts io) [ clojure.string :as string]) )

(defn read-dataset1 []
  (incanter.io/read-dataset "DATFILE.dat" :delim \space))


;; have to get rid of the notes crap first
(defn remove-junk-from [line]
  (let [[_ interesting] (string/split line #"foobar")]
    (if (nil? interesting)
        ""
        (string/trim interesting)
        )
    ))
(defn remove-junk []
  (let [raw (->>
             (slurp "cloc.log")
             (string/split-lines)
             (map remove-junk-from)
             
             (map #(string/split %1 #"\s+"))
                         (remove #(< (count %) 7 ))
            
             )]
    (do    (spit "cloc.log"raw :append false))
    ))
 
(defn select-fields [data]
  (clean-dataset (conj-cols (sel data :cols :Date) (sel data :cols :Locs)))
  )
(defn read-dataset2 []
  (-> 
   (incanter.io/read-dataset "cloc.dat") 
   (col-names [:Hash :Date :whatever :Files :Blank :Comment :Locs])
   (select-fields)
   )
  )


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

(defn create-chart [ds]
  (doto (incanter.charts/time-series-plot :Date :Locs :x-label "Date"  :y-label "Lines of code" :data ds)
    (incanter.charts/add-lines (sel ds :cols :Date) (:fitted (incanter.stats/linear-model (sel ds :cols 1) (sel ds :cols 0))))))

(defn add-interesting-data [chart])

(defn doitall []
  (->> (read-dataset2)
      (clean-dataset)
     ;; (downsample 200)
     ;; (clean-dataset) ;; ooops
      (fix-dates)
      (clean-dataset) ;; double oops
      (create-chart)))

(defn view-chart []
  (view (doitall)))

(defn save-chart [ location]
  (save (doitall ) location :width 800 :height 600)
  )

;; authors bit

(defn read-authors []
  (->> (slurp "/Users/jensmith/Dropbox/legacy codebase/authors.log")
      (string/split-lines)
      (map #(string/split % #"/|&|\+|_and_|\sand\s"))
      (flatten)
      (map string/trim)
      (map string/lower-case)
      (distinct)
      (remove empty?)
      ))

(defn determine-file-type [file]
  (cond
   (re-find #".erb" file) :views
   (re-find #"models\/" file) :models
   (re-find #"inbound/.*/gateway.rb" file)  :inbound-services
   (re-find #"outbound/.*/gateway.rb" file) :outbound-services
   (re-find #"controllers/.*controller.rb" file) :controllers
   (re-find #"helper" file) :helpers))

;; temp rapidftr
(defn read-app-log []
  (->> (slurp "/Users/jensmith/Dropbox/legacy codebase/rapid_ftr_app.log")
       (string/split-lines)
       (map determine-file-type)
       (remove nil?)
       (frequencies)))

;; vital statistics

(defn get-stats []
  (merge (read-app-log)
         { :commits (count (sel (read-dataset1) :cols 0 ) )
          :authors (count (read-authors))
          }))
