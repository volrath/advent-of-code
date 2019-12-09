#!/usr/bin/env clj

(require 'intcode)

(def program (intcode/parse (slurp "input/day5")))


(comment
  (intcode/computer (intcode/parse "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")
                    {:input-fn (constantly 8)})
  )


(println "Part 1:")
(println "------")
(intcode/computer program {:input-fn (constantly 1)})

(println)

(println "Part 2")
(println "------")
(intcode/computer program {:input-fn (constantly 5)})
