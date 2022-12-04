(ns advent-of-code.day-04
  (:require [clojure.string :as str] [clojure.set :as set]))

(defn irange [start end] (range start (inc end)))
(defn part-1
  "Day 04 Part 1"
  ([input pred]
   (->> input
        (str/split-lines)
        (map #(str/split % #","))
        (map (fn [elves] (map #(set (apply irange (map read-string (str/split % #"-")))) elves)))
        (filter pred)
        count))
  ([input]
   (part-1 input (fn [[elf1 elf2]] (or (set/subset? elf1 elf2) (set/superset? elf1 elf2))))
   ))

(defn part-2
  "Day 04 Part 2"
  [input]
  (part-1 input (fn [[elf1 elf2]] (seq (set/intersection elf1 elf2)))))
