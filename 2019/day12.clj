#!/usr/bin/env clj

(require '[clojure.set :as set])
(require '[clojure.string :as string])
(require '[clojure.math.numeric-tower :as math])


(defn parse-input
  [input-str]
  (letfn [(boot-moon [moon-str]  ;; => {:position {:x :y :z} :velocity {:x :y :z}}
            (->> moon-str
                 (re-matches #"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>")
                 rest
                 (map #(Integer/parseInt %))
                 (zipmap [:x :y :z])
                 (hash-map :velocity {:x 0 :y 0 :z 0} :position)))]
    (->> (string/split input-str #"\n")
         (mapv boot-moon)
         (zipmap [:io :europa :ganymede :callisto])
         (into (sorted-map)))))


(def moons (parse-input (slurp "input/day12")))


(defn step
  [moons]
  (letfn [(axis [a moons]
            (map (comp a :position second) moons))
          (apply-gravity [[moon-name {:keys [position velocity] :as specs}]]
            (let [new-velocity (let [{px :x py :y pz :z} position
                                     {vx :x vy :y vz :z} velocity]
                                 {:x (apply + vx (map #(compare % px) (axis :x (dissoc moons moon-name))))
                                  :y (apply + vy (map #(compare % py) (axis :y (dissoc moons moon-name))))
                                  :z (apply + vz (map #(compare % pz) (axis :z (dissoc moons moon-name))))})]
              (assoc specs :velocity new-velocity)))
          (apply-velocity [{:keys [position velocity] :as specs}]
            (let [{px :x py :y pz :z} position
                  {vx :x vy :y vz :z} velocity]
              (assoc specs :position {:x (+ px vx) :y (+ py vy) :z (+ pz vz)})))]
    (into (sorted-map) (map (juxt first (comp apply-velocity apply-gravity)))
          moons)))


(defn total-energy
  [moons]
  (letfn [(moon-total [moon]
            (* (apply + (map math/abs (vals (:position moon))))
               (apply + (map math/abs (vals (:velocity moon))))))]
    (apply + (map moon-total (vals moons)))))


;; Part 1
;; -----------------------------------------------------------------------------

(->> moons
     (iterate step)
     (take (inc 1000))
     last
     total-energy
     (println "Part 1:"))


;; Part 2
;; -----------------------------------------------------------------------------
;; Solved using help from
;; https://www.reddit.com/r/adventofcode/comments/e9jxh2/help_2019_day_12_part_2_what_am_i_not_seeing/

(defn axis-cycle
  [moons axis]
  (map #(vector (get-in % [:position axis]) (get-in % [:velocity axis]))
       (vals moons)))


(defn dim-cycles
  [moons]
  (loop [i           0
         repetitions {}
         track       {:x #{} :y #{} :z #{}}
         moons       moons]
    (let [to-track (set/difference #{:x :y :z} (set (keys repetitions)))]
      (if (empty? to-track)
        repetitions
        (let [cycles (map #(let [cycle (axis-cycle moons %)]
                             (hash-map :axis %
                                       :cycle cycle
                                       :repeated? (contains? (get track %) cycle)))
                          to-track)
              new-repetitions (reduce #(cond-> %1
                                         (:repeated? %2) (assoc (:axis %2) i))
                                      repetitions
                                      cycles)]
          (recur (inc i)
                 new-repetitions
                 (reduce #(if (:repeated? %2)
                            (dissoc %1 (:axis %2))
                            (update %1 (:axis %2) conj (:cycle %2)))
                         track
                         cycles)
                 (step moons)))))))


(->> (dim-cycles moons)  ;; => {:y 113028, :z 231614, :x 268296}
     vals
     (reduce math/lcm)
     (println "Part 2:"))
