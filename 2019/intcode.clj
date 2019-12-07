(ns intcode)

(require '[clojure.string :as string])


(defn binary-op [op-fn]
  (fn [state x y rp]
    (-> state
        (assoc-in [:program rp] (op-fn x y))
        (update :ip + 4))))


(defn input-op [input-fn]
  (fn [state rp]
    (-> state
        (assoc-in [:program rp] (input-fn))
        (update :ip + 2))))


(defn output-op [output-fn]
  (fn [state x]
    (output-fn x)
    (update state :ip + 2)))


(defn jump-op [cond-fn]
  (fn [{:keys [ip] :as state} clause np]
    (assoc state :ip (if (cond-fn clause)
                       np
                       (+ ip 3)))))


(defn comp-op [comp-fn]
  (fn [state x y rp]
    (-> state
        (update :ip + 4)
        (assoc-in [:program rp] (if (comp-fn x y)
                                  1
                                  0)))))


(defn computer
  ([program]
   (computer program {}))
  ([program {:keys [input-fn output-fn]
             :or   {input-fn read-line output-fn println}}]
   (let [op-specs {1 [(binary-op +) [:eval :eval :not-eval]]
                   2 [(binary-op *) [:eval :eval :not-eval]]
                   3 [(input-op input-fn) [:not-eval]]
                   4 [(output-op output-fn) [:eval]]
                   5 [(jump-op (comp not zero?)) [:eval :eval]]
                   6 [(jump-op zero?) [:eval :eval]]
                   7 [(comp-op <) [:eval :eval :not-eval]]
                   8 [(comp-op =) [:eval :eval :not-eval]]}]
     (loop [{:keys [program ip] :as state}
            {:program program
             :ip      0}]
       (let [opcode (get program ip)]
         (when-not (or (nil? opcode) (= opcode 99))
           (let [[op-fn params-desc] (op-specs (mod opcode 10))
                 raw-params          (subvec program (inc ip) (+ 1 ip (count params-desc)))
                 params-modes        (->> opcode str reverse (drop 2) (map {\0 :pos \1 :imm}) vec)
                 params              (map (fn [param-pos param-val]
                                            (if (and (= (get params-desc param-pos) :eval)
                                                     (= (get params-modes param-pos :pos) :pos))
                                              (get program param-val)
                                              param-val))
                                          (range)
                                          raw-params)]
             (recur (apply op-fn state params)))))))))


(defn parse
  [program-string]
  (mapv #(Integer/parseInt (string/trim-newline %))
        (string/split program-string #",")))


(comment
  (let [output (volatile! [])]
    (computer (parse "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")
              {:input-fn (constantly 8)
               :output-fn #(vswap! output conj %)})
    @output)

  (let [output (volatile! [])]
    (computer (parse (slurp "input/day5"))
              {:input-fn (constantly 5)
               :output-fn #(vswap! output conj %)})
    @output)
  )