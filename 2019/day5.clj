#!/usr/bin/env clj

(require '[clojure.string :as string])


(defn parse-program
  [program-string]
  (mapv #(Integer/parseInt (string/trim-newline %))
        (string/split program-string #",")))


(def program
  (parse-program (slurp "input/day5")))


(def ops->param-count
  {1 3
   2 3
   3 1
   4 1
   5 2
   6 2
   7 3
   8 3})


(defn annotate-param-mode
  [opcode param-position param-value]
  (let [modes (->> opcode str reverse (drop 2) (map {\0 :pos \1 :imm}) vec)]
    {:og-val param-value
     :mode   (get modes param-position :pos)}))


(defn eval-in-mode
  [program {:keys [og-val mode] :as _param}]
  (if (= mode :imm)
    og-val
    (get program og-val)))


(defn plus
  [{:keys [program] :as state} x y rp]
  (let [[x y] (map (partial eval-in-mode program) [x y])]
    (-> state
        (assoc-in [:program (:og-val rp)] (+ x y))
        (update :ip + 1 (ops->param-count 1)))))


(defn mult
  [{:keys [program] :as state} x y rp]
  (let [[x y] (map (partial eval-in-mode program) [x y])]
    (-> state
        (assoc-in [:program (:og-val rp)] (* x y))
        (update :ip + 1 (ops->param-count 2)))))


(defn input
  [input state {:keys [og-val]}]
  (-> state
      (assoc-in [:program og-val] input)
      (update :ip + 1 (ops->param-count 3))))


(defn output
  [{:keys [program] :as state} x]
  (println "Output:" (eval-in-mode program x))
  (update state :ip + 1 (ops->param-count 4)))


(defn jump-if-true
  [{:keys [program ip] :as state} clause np]
  (assoc state :ip (if-not (zero? (eval-in-mode program clause))
                     (eval-in-mode program np)
                     (+ 1 ip (ops->param-count 5)))))


(defn jump-if-false
  [{:keys [program ip] :as state} clause np]
  (assoc state :ip (if (zero? (eval-in-mode program clause))
                     (eval-in-mode program np)
                     (+ 1 ip (ops->param-count 6)))))


(defn less-than
  [{:keys [program] :as state} x y rp]
  (-> state
      (update :ip + 1 (ops->param-count 7))
      (assoc-in [:program (:og-val rp)] (if (< (eval-in-mode program x) (eval-in-mode program y))
                                          1
                                          0))))


(defn equals
  [{:keys [program] :as state} x y rp]
  (-> state
      (update :ip + 1 (ops->param-count 8))
      (assoc-in [:program (:og-val rp)] (if (= (eval-in-mode program x) (eval-in-mode program y))
                                          1
                                          0))))


(defn intcode-computer
  [program ipt]
  (loop [{:keys [program ip] :as state}
         {:program program
          :ip      0}]
    (let [opcode (get program ip)]
      (when-not (or (nil? opcode) (= opcode 99))
        (let [op-num      (mod opcode 10)
              op          (case op-num
                            1 plus
                            2 mult
                            3 (partial input ipt)
                            4 output
                            5 jump-if-true
                            6 jump-if-false
                            7 less-than
                            8 equals)
              op-params-p (+ 1 ip (ops->param-count op-num))
              raw-params  (subvec program (inc ip) op-params-p)
              params      (map (partial annotate-param-mode opcode) (range (count raw-params)) raw-params)
              new-state   (apply op state params)]
          (recur new-state))))))

(comment
  (intcode-computer (parse-program "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9") 1)
  (intcode-computer (parse-program "3,3,1105,-1,9,1101,0,0,12,4,12,99,1") 1)
  (intcode-computer (parse-program "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99") 1)
  )

(comment
  (do
    (println "Part 1:")
    (println "------")
    (intcode-computer program 1))

  (println)

  (do
    (println "Part 2")
    (println "------")
    (intcode-computer program 5))
  )
