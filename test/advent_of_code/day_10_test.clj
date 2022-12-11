(ns advent-of-code.day-10-test
  (:require [advent-of-code.day-10 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(deftest part1
  (let [expected 13140]
    (is (= expected (part-1 (slurp (resource "day-10-example.txt")))))))

(deftest part2
  (let [expected "##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######....."]
    (is (= expected (part-2 (slurp (resource "day-10-example.txt")))))))
