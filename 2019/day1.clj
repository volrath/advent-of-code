#!/usr/bin/env clj

(require '[clojure.string :as string])

(def input
  (map #(Integer/parseInt %)
       (string/split (slurp "input/day1") #"\n")))

;; Part 1
;; -----------------------------------------------------------------------------

(defn fuel* [mass]
  (-> mass
      (/ 3)
      int
      (- 2)))

(reduce + (map fuel* input))


;; Part 2
;; -----------------------------------------------------------------------------

(defn fuel [mass]
  (let [fuel-mass (fuel* mass)]
    (if (pos? fuel-mass)
      (+ fuel-mass (fuel fuel-mass))
      0)))

(reduce + (map fuel input))
