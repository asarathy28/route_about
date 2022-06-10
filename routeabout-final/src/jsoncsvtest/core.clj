(ns jsoncsvtest.core
  (:require [clojure.data.json :as json]))


;read the response data from the postman get requests
(def file (json/read-str (slurp "src/GraphHopperpostman_collection.json")))





;get rid of extra data in the file, turn file into a list of just the segment path info
(def info (get file "variable"))
(def string2 (get (nth info 4) "value"))
(def string3 (json/read-str string2))
(def string4 (for [x string3]
               (get x "paths")))

; lists is a list of maps of segment info
(def lists(for [x string4]
            (nth x 0)))

; number each segment
(def segments(zipmap (vec (range (count lists)))lists))

; get only the variable information we need from each segment (distance, elevation, and surface), renumber them
(def segments1 (zipmap (vec (range (count segments))) (for [x (range (count segments))]
                                                        (select-keys (get segments x) ["bbox" "distance" "ascend" "descend" "details"]))))





