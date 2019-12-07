#!/usr/bin/env clj

(require 'intcode)

(def program (intcode/parse (slurp "input/day5")))


(println "Part 1:")
(println "------")
(intcode/computer program {:input-fn (constantly 1)})

(println)

(println "Part 2")
(println "------")
(intcode/computer program {:input-fn (constantly 5)})
