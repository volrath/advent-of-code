(ns intcode
  (:require [clojure.string :as string]))


(defn mem-get
  [program idx]
  (get program idx 0))


(defn mem-set
  "Automatically extend vector size if `idx` is out of range."
  [program idx new-val]
  (if (> (count program) idx)
    (assoc program idx new-val)
    (vec (concat program (repeat (- idx (count program)) 0) [new-val]))))


(defn binary-op [op-fn]
  (fn [state x y rp]
    (-> state
        (update :program mem-set rp (op-fn x y))
        (update :ip + 4))))


(defn input-op [input-fn]
  (fn [state rp]
    (-> state
        (update :program mem-set rp (input-fn))
        (update :ip + 2))))


(defn output-op [output-fn]
  (fn [state x]
    (output-fn x)
    (-> state
        (update :ip + 2)
        (update :output conj x))))


(defn jump-op [cond-fn]
  (fn [{:keys [ip] :as state} clause np]
    (assoc state :ip (if (cond-fn clause)
                       np
                       (+ ip 3)))))


(defn comp-op [comp-fn]
  (fn [state x y rp]
    (-> state
        (update :ip + 4)
        (update :program mem-set rp (if (comp-fn x y)
                                      1
                                      0)))))


(defn rebase-op [state nb]
  (-> state
      (update :ip + 2)
      (update :rel-base + nb)))


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
                   8 [(comp-op =) [:eval :eval :not-eval]]
                   9 [rebase-op [:eval]]}]
     (loop [{:keys [program ip rel-base] :as state}
            {:program  program
             :ip       0
             :rel-base 0}]
       (let [opcode (get program ip)]
         (if (or (nil? opcode) (= opcode 99))
           (:output state)
           (let [[op-fn params-desc] (op-specs (mod opcode 10))
                 raw-params          (subvec program (inc ip) (+ 1 ip (count params-desc)))
                 params-modes        (->> opcode str reverse (drop 2) (map {\0 :pos \1 :imm \2 :rel}) vec)
                 params              (map (fn [param-pos param-val]
                                            (let [param-desc (get params-desc param-pos)
                                                  param-mode (get params-modes param-pos :pos)]
                                              (cond->> param-val
                                                (= param-mode :rel)
                                                (+ rel-base)
                                                (and (not= param-mode :imm) (= param-desc :eval))
                                                (mem-get program))))
                                          (range (count raw-params))
                                          raw-params)]
             (recur (apply op-fn state params)))))))))


(defn parse
  [program-string]
  (mapv #(Long/parseLong %)
        (string/split (string/trim program-string) #",")))
