#!/usr/bin/env clj


(def part-1-solution
  "let's not calculate thing twice..."
  (atom nil))


;; Part 1
;; -----------------------------------------------------------------------------

(let [increasing-digits? #(apply <= (map int %))
      with-double?       #(> (count %) (count (set %)))]
  (->> (range 271973 785961)
       (map str)
       (filter (every-pred increasing-digits? with-double?))
       (reset! part-1-solution)
       count
       (println "Part 1:")))


;; Part 2
;; -----------------------------------------------------------------------------

(let [no-part-or-larger-group? #(contains? (set (vals (frequencies %))) 2)]
  (->> @part-1-solution
       (filter no-part-or-larger-group?)
       count
       (println "Part 2:")))
