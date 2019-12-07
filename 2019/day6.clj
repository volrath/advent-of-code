#!/usr/bin/env clj

(require '[clojure.string :as string])
(require '[clojure.data :as data])


(def sample-input
  "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")


(defn parse-input
  [input]
  (map #(into [] (map keyword) (string/split % #"\)"))
       (string/split input #"\n")))


(defn inverted-relations
  [input]
  (map (comp vec reverse) input))


(defn object-parents
  [input]
  (let [inverted-relations (into {} (map (comp vec reverse) input))]
    (into {}
          (map (juxt identity #(take-while some? (iterate (partial get inverted-relations) %)))
               (keys inverted-relations)))))


;; Part 1
;; -----------------------------------------------------------------------------

(println "Part 1:"
         (->> (slurp "input/day6")
              parse-input
              object-parents
              (reduce-kv #(+ %1 (dec (count %3))) 0)))


;; Part 2
;; -----------------------------------------------------------------------------

;; A really inefficient way to solve it :)
(println "Part 2:"
         (let [{me :YOU santa :SAN} (-> (slurp "input/day6")
                                        parse-input
                                        object-parents)
               [me-diff santa-diff] (data/diff (set me) (set santa))]
           (+ (dec (count me-diff)) (dec (count santa-diff)))))


(comment
  (->> sample-input
       parse-input
       object-parents
       #_(reduce #(+ %1 (dec (count %2))) 0))

  (let [{me :YOU santa :SAN} (-> (str sample-input "\nK)YOU\nI)SAN")
                                 parse-input
                                 object-parents)
        [me-diff santa-diff] (data/diff (set me) (set santa))]
    (+ (dec (count me-diff)) (dec (count santa-diff))))
  )
