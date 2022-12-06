(ns advent-of-code.day-06
  (:require [clojure.string :as str]))

(defn part-1
  "Day 06 Part 1"
  ([input size]
  (->> input
       (char-array)
       (partition size 1)
       (split-with #(not= (count (distinct %1)) (count %1)))
       (first)
       (count)
       (+ size)
       ))
  ([input] (part-1 input 4)))

(defn part-2
  "Day 06 Part 2"
  [input]
  (part-1 input 14))
