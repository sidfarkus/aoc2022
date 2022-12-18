(ns advent-of-code.day-18 
  (:require [advent-of-code.util :refer [neighbors-voxel supersplit]]))

(defn part-1
  "Day 18 Part 1"
  [input]
  (let [parsed (supersplit [#"\n" #","] #(Integer/parseInt %) input)
        space (into (hash-map) (map #(vector % 1) parsed))]
    (reduce 
     + 
     (map (fn [[x y z]]
            (- 6 (count (filter some? (map (partial nth (neighbors-voxel space x y z)) [8 9 11 13 15 25])))))
          parsed))
    ))

(defn part-2
  "Day 18 Part 2"
  [input]
  input)
