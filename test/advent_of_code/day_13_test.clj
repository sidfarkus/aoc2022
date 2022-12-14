(ns advent-of-code.day-13-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code.day-13 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1
  (let [expected 13]
    (is (= expected (part-1 (slurp (resource "day-13-example.txt")))))))

(deftest part2
  (let [expected 140]
    (is (= expected (part-2 (slurp (resource "day-13-example.txt")))))))
