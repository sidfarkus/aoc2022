(ns advent-of-code.day-01
  (:require [clojure.string :as str]))

(defn totals [input] (map #(reduce + (map read-string (str/split %1 #"\n"))) (str/split input #"\n\n")))

(defn part-1
  "Day 01 Part 1"
  [input]

  (apply max (totals input))
 )


(defn part-2
  "Day 01 Part 2"
  [input]

  (reduce + (take-last 3 (sort (totals input))))
  )
