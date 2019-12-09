#!/usr/bin/env clj

(require 'intcode)

(def boost (intcode/parse (slurp "input/day9")))

(comment
  (->> (intcode/computer (intcode/parse "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"))
       reverse
       (clojure.string/join ","))
  (intcode/computer (intcode/parse "1102,34915192,34915192,7,4,7,99,0"))
  (intcode/computer (intcode/parse "104,1125899906842624,99"))
  )


(println "Part 1:")
(intcode/computer boost {:input-fn (constantly 1)})


(println "Part 2:")
(intcode/computer boost {:input-fn (constantly 2)})
