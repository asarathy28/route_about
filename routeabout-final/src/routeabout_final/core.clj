(ns routeabout-final.core
  (:require [jsoncsvtest.core :as data]))

;;Copyright © 2014 Thomas Meier

;;Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
;; The assumption is that we are working on a perfect sphere. Obviously,
;; Earth is not a perfect sphere. There will be some discrepencies, typically
;; overestimation, since the great circle will not conform to a rough
;; surface.


;; ------------ Functions for Distance Calculation from Coordinates -----------------

(def earth-radius "In kilometers" 6372.8)

(defn- sin2
  "Sine squared"
  [theta]
  (* (Math/sin theta) (Math/sin theta)))

(defn- alpha
  "Trigonometric calculations for use in haversine."
  [lat1 lat2 delta-lat delta-long]
  (+ (sin2 (/ delta-lat 2))
     (* (sin2 (/ delta-long 2)) (Math/cos lat1) (Math/cos lat2))))

(defn haversine
  "Distance in km between two points of lat/long. Supply two maps of latitutde and longitude."
  [{lat1 :latitude long1 :longitude}
   {lat2 :latitude long2 :longitude}]
  (let [delta-lat (Math/toRadians (- lat2 lat1))
        delta-long (Math/toRadians (- long2 long1))
        lat1 (Math/toRadians lat1)
        lat2 (Math/toRadians lat2)]
    (* earth-radius 2
       (Math/asin (Math/sqrt (alpha lat1 lat2 delta-lat delta-long))))))

;; ------------ End Functions for Distance Calculation from Coordinates -----------------




(defn coord-to-map [coord]
  "Returns the coordinates in haversine input format."
  {:latitude (nth coord 1)
   :longitude (nth coord 0)})

#_(defn coord-to-map [coord start]
    "Returns the coordinates of start (start = true) or end (start = false) point of segment
    in haversine input format."
    {:latitude (nth coord (if start 1 3))
     :longitude (nth coord (if start 0 2))})


;;NOTE: Genome = vector of (segment #, direction true or false)

(defn find-neighbors [coord]
  "Returns all segments with start point within x km of the current coordinates."
  (filter #(< (haversine
                (coord-to-map coord)
                (coord-to-map (vec (take 2 (get (get data/segments1 (first %)) "bbox")))))
              0.08)
          data/segments1))

#_(find-neighbors [-72.568, 42.4])

#_(filter #(< (get (get data/segments1 (first %)) "distance") 10) data/segments1)



(defn rand-neighbor [curr]
  "Picks a random neighboring segment beginning near the current coordinates.
  Returns the segment number."
  (let [coordinates (get (get data/segments1 curr) "bbox")]
    (rand-nth (remove #{curr}
                      (keys (find-neighbors (vector (nth coordinates 2) (nth coordinates 3))))))))

#_(rand-neighbor 1)

(defn abs [n] (max n (- n)))

#_(rand-neighbor :amherst)



(defn coord-sg-dist [coordinates, i]
  "Function for nearest-segment function.
  Returns distance between coordinates and starting point of i."
  (haversine
    (coord-to-map coordinates)
    (coord-to-map (vec (take 2 (get (second i) "bbox"))))))

(defn nearest-segment [coordinates]
  "Returns the segment number of segment starting nearest to the given coordinates."
  (first
    (reduce (fn [i1 i2]
              (if (< (coord-sg-dist coordinates, i1)
                     (coord-sg-dist coordinates, i2))
                i1
                i2))
            data/segments1)))

#_(nearest-segment [-72.562734 42.40667])

(defn build-path [origin]
  "Builds random path, given the starting coordinates origin."
  (loop
    [path []
     curr (nearest-segment origin)
     count (+ (rand-int 15) 2)]
    (if (< count 1)
      path
      (recur (conj path curr)
             (rand-neighbor curr)
             (- count 1)))))

#_(build-path [0,0])

#_(build-path :amherst)

(defn total-distance [genome]
  "Calculates the total distance of the genome (first attribute).
  Genome is a vector of segment numbers."
  (reduce + (map #(get (get data/segments1 %) "distance") genome)))



(defn sum-dup [numbers]
  (reduce + (vals (filter (fn [[k v]] (> v 1)) (frequencies numbers)))))

#_(vals (filter (fn [[k v]] (> v 1)) (frequencies t-list)))
#_(sum-dup t-list)

#_(defn error [genome target]
    (if (< (count genome) 2)
      1000000000
      (let[dist-score (abs (- (total-distance genome) target))
           loop-score (* 0.62137 (haversine (coord-to-map (get coord (first genome))) (coord-to-map (get coord (last genome)))))
           repeat-score (- (sum-dup genome) 1)]
        (+ dist-score (* 2 loop-score) (* 5 repeat-score)))))

(defn get-coord [sg-num, start]
  "Returns the coordinates of start (1) or end (0) point of segment given the segment number."
  (let [coords (get (get data/segments1 sg-num) "bbox")]
    (if (= start 1)
      (vec (take 2 coords))
      (vector (nth coords 2) (nth coords 3)))))

#_(defn error [genome target]
    (if (< (count genome) 2)
      1000000000
      (let[dist-score (abs (- (total-distance genome) target))
           loop-score (* 0.62137 (haversine
                                   (coord-to-map (get-coord (first genome) 1))
                                   (coord-to-map (get-coord (last genome) 0))))
           repeat-score (- (sum-dup genome) 1)]
        (+ dist-score (* 2 loop-score) (* 5 repeat-score)))))

(defn avg-coordinate [genome]
  "Returns the average coordinate of all start and end points in the genome."
  (let [all-coords (concat (map #(get-coord % 0) genome)
                           (map #(get-coord % 1) genome))]
    (vector
      (/ (reduce + (map #(first %) all-coords)) (count all-coords))
      (/ (reduce + (map #(second %) all-coords)) (count all-coords)))))

(defn error [genome target]
  "error function which takes into account the difference between the route's distance and the target distance (dist-score),
  number of segments repeated (repeat-score),
  and the distance from one segment to another (venture-score)"
  (if (< (count genome) 2)
    1000000000
    (let[dist-score (abs (- (total-distance genome) target))
         repeat-score (- (sum-dup genome) 1)
         venture-score (- 1 (haversine
                              (coord-to-map (get-coord (last genome) 0))
                              (coord-to-map (avg-coordinate genome))))]
      (+ dist-score (* 5 repeat-score) (* 10 venture-score)))))


#_(error (build-path [0,0]) 45)

#_(error (build-path :amherst) 45)



(defn new-individual [origin target]
  "Returns a new, random individual."
  (let [genome (build-path origin)]
    {:genome genome
     :error  (error genome target)}))


(defn best [individuals]
  "Returns the best of the given individuals."
  (reduce (fn [i1 i2]
            (if (< (:error i1) (:error i2))
              i1
              i2))
          individuals))
#_(new-individual :amherst 17)

(defn select [population]
  "Returns an individual selected from population using a tournament."
  (best (repeatedly 2 #(rand-nth population))))

(defn mutate [path]
  "Returns a mutated copy of genome."
  (if (< (rand-int 2) 1)
    (if (> (count path) 2)
      (pop path))
    (conj path (rand-neighbor (peek path)))))

#_(mutate [1 2 3 4 5])

#_(mutate test)


(defn make-child [population target]
  "Returns a new, evaluated child, produced by mutating a parent selected from the given population."
  (let [new-genome (mutate (:genome (select population)))]
    {:genome new-genome
     :error  (error new-genome target)}))

#_(defn make-child [population target]
    "Returns a new, evaluated child, produced by crossing over parents
    that are selected from the given population."
    (let [new-genome (vec (crossover (select population) (select population)))]
      {:genome new-genome
       :error  (error new-genome target)}))

#_(defn make-child [population target]
    "Returns a new, evaluated child, produced by mutating the result
    of crossing over parents that are selected from the given population."
    (let [new-genome (mutate (vec (crossover (select population) (select population))))]
      {:genome new-genome
       :error  (error new-genome target)}))

(defn report [generation population]
  "Prints a report on the status of the population at the given generation."
  (let [current-best (best population)]
    (println {:generation   generation
              :best-error   (:error current-best)
              :diversity    (float (/ (count (distinct population))
                                      (count population)))
              :total-distance (total-distance (:genome current-best))
              :best-genome  (:genome current-best)})))

(defn evolve [population-size generations origin target]
  "Evolves a genome that tries to find the optimal running route based on attributes like distance and loopiness.
  The function takes in a population size, number of generations to be evolved, the starting point and the target distance (in meters).
  The final genome returns a list of segments that create a route"
  (loop [population (repeatedly population-size #(new-individual origin target))
         generation 0]
    (report generation population)
    (if (>= generation generations)
      (best population)
      (recur (conj (repeatedly (dec population-size) #(make-child population target))
                   (best population))
             (inc generation)))))

#_(evolve 200 100 [5,42] 1000)
#_(evolve 100 100 [0,0] 1500)

;; ------------- Plotting Results on a Map -------------------

#_(def t-list [:amherst :northampton :ashfield :greenfield :north-adams :greenfield :north-adams :greenfield :ashfield :northampton :greenfield :ashfield :greenfield :northampton :amherst])

#_(defn plot-points [genome]
    (print
      (flatten (map #(str (first %) "," (last %) ",red,circle,\"x\"\n") (map #(get coord %) genome)))))

#_(plot-points t-list)

(defn plot-points [genome]
  (print
    (flatten (map #(str (last (get-coord % 1)) "," (first (get-coord % 1)) ",red,circle,\"x\"\n"
                        (last (get-coord % 0)) "," (first (get-coord % 0)) ",red,circle,\"x\"\n")
                  genome))))

;; ------------- End Plotting Results on a Map -------------------



;; ------------ Functions for Crossover -----------------

;; Find subpaths that begin & end near each other

(defn choose-index [genome location]
  "Returns all indices in the genome that corresponds to the location."
  (rand-nth (map first
                 (filter #(= (second %) location)
                         (map-indexed vector genome)))))

(defn segmentize [genome i1 i2]
  "Divides a vector into three parts at indices i1 and i2. The middle segment contains i1 and i2."
  (list
    (first (split-at i1 genome))
    (first (split-at (+ (- i2 i1) 1) (second (split-at i1 genome))))
    (second (split-at (+ (- i2 i1) 1) (second (split-at i1 genome))))))

(defn crossover [parent1 parent2]
  "Returns the genome of a child produced by crossing over the paths of two genomes between two shared locations."
  ;; Returns one of the parents if unsuccessful
  ;; Swapping choices are made by randomly choosing two locations first then, if location(s) repeated, randomly choosing one occurrence
  (let [g1 (:genome parent1)
        g2 (:genome parent2)
        overlap (shuffle (vec (clojure.set/intersection (set g1) (set g2))))
        all-overlaps (concat (filter #(some #{%} overlap) g1) (filter #(some #{%} overlap) g2))]
    (if (> (count overlap) 1)
      (let [loc1 (rand-nth all-overlaps)
            loc2 (rand-nth (filter #(not (= % loc1)) all-overlaps))
            i1 (choose-index g1 loc1)
            i2 (choose-index g1 loc2)
            j1 (choose-index g2 loc1)
            j2 (choose-index g2 loc2)
            g1-seg (if (< i1 i2) (segmentize g1 i1 i2) (segmentize g1 i2 i1))
            g2-seg (if (< j1 j2) (segmentize g2 j1 j2) (segmentize g2 j2 j1))]
        (if (> (* (- i2 i1) (- j2 j1)) 0)
          (rand-nth (list (concat (first g1-seg) (second g2-seg) (nth g1-seg 2))
                          (concat (first g2-seg) (second g1-seg) (nth g2-seg 2))))
          (rand-nth (list (concat (first g1-seg) (reverse (second g2-seg)) (nth g1-seg 2))
                          (concat (first g2-seg) (reverse (second g1-seg)) (nth g2-seg 2))))))
      (rand-nth (list g1 g2)))))

#_(crossover (new-individual :amherst 17) (new-individual :amherst 17))

;; ------------ End Functions for Crossover -----------------



;;----------- Functions Using Cities Instead of Segments -----------
;; Clojure code for an evolutionary algorithm for dummy data of western Mass cities


#_(def all-cities [:amherst :northampton :greenfield :north-adams :springfield :holyoke :ashfield :belchertown])

#_(def coord {:north-adams	[42.70091, -73.10871]
              :ashfield	[42.52647, -72.78842]
              :greenfield	[42.58791, -72.59941]
              :northampton	[42.32508, -72.6412]
              :amherst	[42.37332, -72.51971]
              :holyoke	[42.20425, -72.6162]
              :belchertown	[42.27703, -72.40088]
              :springfield	[42.10148, -72.58981]})

#_(def city-neighbors {:north-adams [:greenfield	:ashfield]
                       :ashfield  [:north-adams :greenfield	:northampton]
                       :greenfield  [:north-adams :ashfield :northampton]
                       :northampton [:greenfield	:ashfield	:amherst]
                       :amherst [:northampton	:belchertown]
                       :holyoke [:northampton :belchertown :springfield]
                       :belchertown [:amherst	:holyoke]
                       :springfield [:holyoke]})

#_(def test-coord (get (get data/segments1 (rand-int (count data/segments1))) "bbox"))
#_(haversine (coord-to-map test-coord true) (coord-to-map test-coord false))


;; segment distance
#_(def distances {#{:north-adams	:greenfield}	41.8
                  #{:north-adams	:ashfield}	29.8
                  #{:ashfield	:greenfield}	20.2
                  #{:ashfield	:northampton}	20.2
                  #{:greenfield	:northampton}	19.4
                  #{:northampton	:amherst}	7.8
                  #{:northampton	:holyoke}	11.3
                  #{:amherst	:belchertown}	9.9
                  #{:belchertown	:holyoke}	13.3
                  #{:holyoke	:springfield}	8.8})

;;(defn total-distance [cities]
;;  (reduce + (map #(get distances (set %))
;;                 (partition 2 1 cities))))

;; the distance of the path specified by the order in all-cities
#_(total-distance all-cities)

;; a random other order
#_(total-distance (shuffle all-cities))

#_(defn rand-neighbor [curr]
    "pick random neighbor"
    (rand-nth (get city-neighbors curr)))

#_(defn build-path  [origin]
    "builds random path, given origin"
    (loop
      [path []
       curr origin
       count (+ (rand-int 15) 2) ]
      (if (< count 1)
        path
        (recur (conj path curr)
               (rand-neighbor curr)
               (- count 1)))))

#_(defn total-distance [cities]
    "calculates the total distance (first attribute)"
    (reduce + (map #(get distances (set %))
                   (partition 2 1 cities))))

#_(def test [:amherst :northampton :ashfield :northampton :amherst])
#_(total-distance test)
#_(total-distance (build-path :amherst))#_(defn total-distance [cities]
                                            "calculates the total distance (first attribute)"
                                            (reduce + (map #(get distances (set %))
                                                           (partition 2 1 cities))))

#_(def test [:amherst :northampton :ashfield :northampton :amherst])
#_(total-distance test)
#_(total-distance (build-path :amherst))
;;----------- End Functions Using Cities Instead of Segments -----------





