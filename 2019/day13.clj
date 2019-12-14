(ns day13
  (:require intcode
            [clojure.core.async :as a]
            [quil.core :as q]))


(def game (intcode/parse (slurp "input/day13")))


;; Part 1
;; -----------------------------------------------------------------------------

(def initial-out (volatile! nil))


(->> (intcode/computer game {:output-fn identity})
     (vreset! initial-out)  ;; Just to not recalculate this in Part 2.
     (take-nth 3)
     (filter #{2})
     count
     (println "Part 1:"))


;; Part 2
;; -----------------------------------------------------------------------------

(println "Part 2:")

(def tile-size 20)

(defn computer-out->tiles
  [out]
  (->> out
       reverse
       (partition-all 3)
       (reduce (fn [m [x y id]] (assoc m [x y] id)) {})))


(defn setup
  []
  (q/set-state! :tiles (atom {})
                :paused  true)
  (let [outs      (volatile! nil)
        output-fn (fn [o]
                    (let [outs* (vswap! outs conj o)]
                      (when (= 3 (count outs*))
                        (swap! (q/state :tiles) merge (computer-out->tiles outs*))
                        (vreset! outs nil))))
        input-fn  (fn []
                    (Thread/sleep 75)
                    (compare (ffirst (first (filter #(= 4 (second %)) @(q/state :tiles))))
                             (ffirst (first (filter #(= 3 (second %)) @(q/state :tiles))))))]
    (a/go
      (intcode/computer (assoc game 0 2)
                        {:input-fn  input-fn
                         :output-fn output-fn}))))


(defn draw
  []
  (q/background 240 248 255)
  (q/no-stroke)
  (doseq [[[x y] id] @(q/state :tiles)
          :when (not (and (= x -1) (= y 0)))
          :let [x (* tile-size x)
                y (* tile-size y)]]
    (apply q/fill (case id
                    0 [240 248 255]
                    1 [165 42 42]
                    2 [0 139 139]
                    3 [128 128 0]
                    4 [112 128 144]))
    (if (= id 4)
      (q/ellipse (+ x (/ tile-size 2)) (+ y (/ tile-size 2)) tile-size tile-size)
      (q/rect x y tile-size tile-size)))
  (q/fill 0)
  (q/text (str "Score: " (get @(q/state :tiles) [-1 0])) 10 15))


(q/defsketch arcade
  :title "Arcade Cabinet"
  :setup setup
  :draw draw
  :size [(* tile-size (inc (apply max (take-nth 3 (drop 2 @initial-out)))))
         (* tile-size (inc (apply max (take-nth 3 (drop 1 @initial-out)))))])
