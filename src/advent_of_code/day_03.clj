(ns advent-of-code.day-03
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn value [x] (+ (if (< (int x) 97) 27 1) (str/index-of "abcdefghijklmnopqrstuvwxyz" (str/lower-case x))))
(defn part-1
  "Day 03 Part 1"
  [input]
  (->> input
       (str/split-lines)
       (map #(split-at (/ (.length %1) 2) (char-array %1)))
       (map (fn [[a b]] (first (set/intersection (set a) (set b)))))
       (map value)
       (reduce +)))


(defn part-2
  "Day 03 Part 2"
  [input]
  (->> input
       (str/split-lines)
       (partition 3)
       (map (fn [[a b c]] (first (set/intersection (set a) (set b) (set c)))))
       (map value)
       (reduce +)
       ))
