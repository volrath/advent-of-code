#!/usr/bin/env clj

(require '[clojure.core.async :as a])
(require 'intcode)

(def robot (intcode/parse (slurp "input/day11")))


(defn next-direction
  [current movement]
  (let [clockwise-dirs [[:v 1] [:h 1] [:v -1] [:h -1]]
        current-idx    (.indexOf clockwise-dirs current)
        movement       (if (zero? movement) -1 1)]
    (get clockwise-dirs (mod (+ current-idx movement) 4))))


(defn paint
  [init-panel]
  (let [input  (a/chan)
        output (a/chan)
        finish (a/go
                 (intcode/computer robot {:input-fn  #(a/<!! input)
                                          :output-fn #(a/put! output %)}))]
    (a/>!! input init-panel)
    (loop [hull      {}
           direction [:v 1] ;; vertical: 1 (up) -1 (down) ; horizontal: 1 (right) -1 (left)
           position  [0 0]]
      (let [[color chan] (a/alts!! [output finish])]
        (if (= chan finish)
          hull
          (let [new-hull                (assoc hull position color)
                [d f :as new-direction] (next-direction direction (a/<!! output))
                new-position            (update position
                                                (if (= d :h) 0 1)
                                                +
                                                f)]
            (a/put! input (get new-hull new-position 0))
            (recur new-hull new-direction new-position)))))))

;; Part 1
;; -----------------------------------------------------------------------------

(println "Part 1:" (count (paint 0)))


;; Part 2
;; -----------------------------------------------------------------------------

(println "Part 2 (tilt your head):")
(let [hull (paint 1)
      xs (map ffirst hull)
      ys (map (comp second first) hull)]
  (doseq [x (range (apply min xs) (inc (apply max xs)))]
    (->> (range (apply min ys) (inc (apply max ys)))
         (map (comp {0 \space 1 \X}
                    #(get hull % 0)
                    (partial vector x)))
         (apply str)
         println)))
