#!/usr/bin/env clj

(require '[clojure.string :as string])


(def input (mapv vec (string/split (slurp "input/day10") #"\n")))
(def input-width (count (first input)))
(def input-height (count input))


(defn position
  [index]
  [(mod index input-width) (quot index input-height)])


(def asteroids
  (->> input
       (apply concat)
       (map #(hash-map :position (position %1) :asteroid? (= \# %2)) (range))
       (remove (comp not :asteroid?))
       (map :position)))


(defn line-of-sight
  [[sx sy :as _station] [ax ay :as asteroid]]
  {:position asteroid
   :angle (let [angle (+ (Math/atan2 (- ay sy) (- ax sx)) (/ Math/PI 2))]
            (cond-> angle
              (neg? angle) (+ (* 2 Math/PI))))})


(defn detected-asteroids
  "Returns a map of <angle from station> -> <seq of asteroids in said angle sorted
  by proximity to station, asc>.

  The sorting thing is only needed for the second part."
  [[sx sy :as station]]
  (letfn [(distance [[x y]]
            (Math/sqrt (+ (Math/pow (- sx x) 2) (Math/pow (- sy y) 2))))
          (distance-compare [p1 p2]
            (compare (distance p1) (distance p2)))]
    (->> asteroids
         (remove #{station})
         (map (partial line-of-sight station))
         (group-by :angle)
         (into {} (map (juxt first (comp (partial sort distance-compare)
                                         (partial map :position)
                                         second)))))))

;; Part 1
;; -----------------------------------------------------------------------------

(->> asteroids
     (map (juxt identity (comp count detected-asteroids)))
     (sort-by second >)
     first
     (println "Part 1:"))


;; Part 2
;; -----------------------------------------------------------------------------

(as-> (detected-asteroids [14 17]) $
  (sort-by first $)
  (map second $)
  (nth $ 199)
  (first $)
  (+ (* 100 (first $)) (second $))
  (println "Part 2:" $))
