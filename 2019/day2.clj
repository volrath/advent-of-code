#!/usr/bin/env clj

(require '[clojure.string :as string])

(def program
  (mapv #(Integer/parseInt (string/trim-newline %))
        (string/split (slurp "input/day2") #",")))


;; Part 1
;; -----------------------------------------------------------------------------

(defn intcode-computer
  [program]
  (loop [program program
         ip      0]
    (let [[opcode p1 p2 ri :as _opcode] (subvec program ip (+ ip 4))]
      (if (or (= opcode 99)
              (> ip (count program)))
        program
        (let [op (case opcode 1 + 2 *)]
          (recur (assoc program ri (op (get program p1) (get program p2)))
                 (+ ip 4)))))))

(println "Part 1:"
         (first
          (intcode-computer (-> program
                                (assoc 1 12)
                                (assoc 2 2)))))


;; Part 2
;; -----------------------------------------------------------------------------

(doseq [noun (range 100)
        verb (range 100)]
  (when (= (first (intcode-computer (-> program
                                        (assoc 1 noun)
                                        (assoc 2 verb))))
           19690720)
    (println "Part 2: Valid" noun verb (+ (* 100 noun) verb))))
