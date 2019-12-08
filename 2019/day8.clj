#!/usr/bin/env clj

(require '[clojure.string :as string])

(def input (string/trim (slurp "input/day8")))


;; Part 1
;; -----------------------------------------------------------------------------

(->> input
     (partition (* 25 6))
     (map frequencies)
     (apply min-key #(get % \0))
     (into {} (remove (comp #{\0} first)))  ;; `select-keys` would've broken my rhythm...
     vals
     (apply *)
     (println "Part 1:"))


;; Part 2
;; -----------------------------------------------------------------------------

(let [layers (partition (* 25 6) input)]
  (->> (range (* 25 6))
       (map (fn [idx]
              (first (remove #{\2} (map #(nth % idx) layers)))))
       (map {\0 \space \1 \X})
       (partition 25)
       (map (partial apply str))
       (string/join "\n")
       (println "Part 2:\n")))
