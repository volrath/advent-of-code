#!/usr/bin/env clj

(require '[clojure.core.async :as a])
(require 'intcode)


(def program (intcode/parse (slurp "input/day7")))


(defn permutations  ;; copied from https://stackoverflow.com/a/26076145
  [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))


;; Part 1
;; -----------------------------------------------------------------------------

(defn run-amplifiers
  [program phase-settings]
  (let [signal (volatile! 0)]
    (doseq [setting phase-settings
            :let    [input-call (volatile! -1)
                     get-input (fn [] (get [setting @signal] (vswap! input-call inc)))]]
      (intcode/computer program
                        {:input-fn  get-input
                         :output-fn #(vreset! signal %)}))
    @signal))


(comment
  (run-amplifiers
   (intcode/parse "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
   [4 3 2 1 0])
  (run-amplifiers
   (intcode/parse "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0")
   [0 1 2 3 4])
  (run-amplifiers
   (intcode/parse "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")
   [1 0 4 3 2])
  )


(println "Part 1:" (ffirst (->> (permutations #{0 1 2 3 4})
                                (map (juxt (partial run-amplifiers program) identity))
                                (sort-by first >))))


;; Part 2
;; -----------------------------------------------------------------------------

(defn feedback-amplifiers
  [program [a-ps b-ps c-ps d-ps e-ps]]
  (let [signals {:a (a/chan) :b (a/chan) :c (a/chan) :d (a/chan) :e (a/chan)}
        result  (a/chan)
        run-amplifier (fn [id out-id]
                        (intcode/computer program
                                          {:input-fn  #(a/<!! (get signals id))
                                           :output-fn #(a/put! (get signals out-id) %)}))]
    ;; run
    (a/go (run-amplifier :a :b))
    (a/go (run-amplifier :b :c))
    (a/go (run-amplifier :c :d))
    (a/go (run-amplifier :d :e))
    (a/go (run-amplifier :e :a)
          ;; When `:e` is done, move its last output signal to `result`
          (a/>! result (a/<! (:a signals))))
    ;; Give inputs
    (a/>!! (:a signals) a-ps)
    (a/>!! (:b signals) b-ps)
    (a/>!! (:c signals) c-ps)
    (a/>!! (:d signals) d-ps)
    (a/>!! (:e signals) e-ps)
    (a/>!! (:a signals) 0)
    ;; Wait for the result
    (a/<!! result)))


(comment
  (feedback-amplifiers
   (intcode/parse "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
   [9 8 7 6 5])
  (feedback-amplifiers
   (intcode/parse "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")
   [9 7 8 5 6])
  )


(println "Part 2:" (ffirst (->> (permutations #{5 6 7 8 9})
                                (map (juxt (partial feedback-amplifiers program) identity))
                                (sort-by first >))))
