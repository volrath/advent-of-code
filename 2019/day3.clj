#!/usr/bin/env clj

(require '[clojure.string :as string])


(defn parse-input
  [input-str]
  (as-> input-str $
    (string/split $ #"\n")
    (map #(string/split % #",") $)))


(def lines (parse-input (slurp "input/day3")))


;; Part 1
;; -----------------------------------------------------------------------------

(defn parse-move
  "Parses a line move to a normalized vector.
  i.e. R10 -> [10 0]
       D5  -> [0 -5]"
  [[direction :as line-move]]
  (let [abs-distance (Integer/parseInt (subs line-move 1))]
    (case direction
      \R [abs-distance 0]
      \L [(- abs-distance) 0]
      \U [0 abs-distance]
      \D [0 (- abs-distance)])))


(comment
  (map parse-move (take 10 (first lines)))
  )


(defn v+
  "Vector addition"
  [u v]
  (vec (map + u v)))


(defn lines->segments
  [line]
  (loop [vectors [[nil [0 0]]]
         [line-move & lms] line]
    (if line-move
      (let [[_lp1 lp2] (last vectors)]
        (recur (conj vectors [lp2 (v+ lp2 (parse-move line-move))])
               lms))
      (vec (rest vectors)))))

(comment
  (let [mini-line (take 10 (first lines))]
    {:original mini-line
     :result   (lines->segments mini-line)})
  )


(defn segment-direction
  [[[p1x _p1y] [p2x _p2y]]]
  (if (= p1x p2x)
    :vertical
    :horizontal))


(defn cross-point
  "Returns the crossing point of two segments, or nil"
  [[[u1x u1y] [u2x u2y] :as u] [[v1x v1y] [v2x v2y] :as v]]
  (let [u-dir (segment-direction u)
        v-dir (segment-direction v)]
    (when-not (= u-dir v-dir)
      (cond
        (and (= u-dir :vertical)
             (or (> u1y v1y u2y) (> u2y v1y u1y))
             (or (> v1x u1x v2x) (> v2x u1x v1x))) [u1x v1y]
        (and (= u-dir :horizontal)
             (or (> u1x v1x u2x) (> u2x v1x u1x))
             (or (> v1y u1y v2y) (> v2y u1y v1y))) [v1x u1y]
        :else nil))))


(defn manhattan-distance
  [[x y]]
  (+ (Math/abs x) (Math/abs y)))


(comment
  (cross-point
   ;; u vertical
   [[0 0] [0 3]]   ;; => segment that goes from 0,0 to 0,3
   [[-1 1] [4 1]])  ;; => segment that goes from -1,1 to 4,1  -> cross at [0 1]

  (cross-point
   ;; u horizontal
   [[-2 5] [3 5]]
   [[0 6] [0 0]])
  )


(defn solve-part-1
  [[line-1 line-2]]
  (let [ss1 (lines->segments line-1)
        ss2 (lines->segments line-2)]
    (->> (for [s1 ss1
               s2 ss2]
           (cross-point s1 s2))
         (remove nil?)
         (map manhattan-distance)
         (apply min))))

(comment
  (cross-point [[8 5] [3 5]] [[0 0] [0 7]])
  ;; Result Part 1

  (solve-part-1 (parse-input "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83"))

  (solve-part-1 (parse-input "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))

  (solve-part-1 (parse-input "R8,U5,L5,D3
U7,R6,D4,L4"))
  )

(println "Part 1:" (solve-part-1 lines))


;; Part 2
;; -----------------------------------------------------------------------------

(defn steps-between-segments
  [[[ux uy] [vx vy]]]
  (Math/abs (+ (- ux vx) (- uy vy))))


(defn solve-part-2
  [[line-1 line-2]]
  (let [min-steps (atom Integer/MAX_VALUE)
        ss1 (lines->segments line-1)
        ss2 (lines->segments line-2)]
    (doseq [i (range (count ss1))
            j (range (count ss2))
            :let [s1 (get ss1 i)
                  s2 (get ss2 j)]]
      (when-let [x-point (cross-point s1 s2)]
        (let [ss1' (-> (vec (take (inc i) ss1))
                       (assoc-in [i 1] x-point))
              ss2' (-> (vec (take (inc j) ss2))
                       (assoc-in [j 1] x-point))
              steps (+ (reduce + (map steps-between-segments ss1'))
                       (reduce + (map steps-between-segments ss2')))]
          (when (< steps @min-steps)
            (reset! min-steps steps)))))
    @min-steps))


(comment
  (solve-part-2 (parse-input "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83"))

  (solve-part-2 (parse-input "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))

  (solve-part-2 (parse-input "R8,U5,L5,D3
U7,R6,D4,L4"))
  )

(println "Part 2:" (solve-part-2 lines))
