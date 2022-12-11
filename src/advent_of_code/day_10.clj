(ns advent-of-code.day-10
  (:require [advent-of-code.util :refer [supersplit]]
            [clojure.string :as str]))

(defn part-1
  "Day 10 Part 1"
  ([input num]
   (let [raw (supersplit [#"\n" #" "] input)
         instructions (map #(if (= % '("noop")) "noop" (Integer/parseInt (last %))) raw)]
     (vec (take num (iterate (fn [[[top & tail] x]]
                                 (cond
                                   (= top "noop") [tail x]
                                   (number? top) [(cons [top] tail) x]
                                   :else [tail (+ x (first top))]))
                             [instructions 1])))))
   ([input]
    (let [scores (part-1 input 221)] (reduce + (map #(* % (last (get scores (dec %)))) '(20 60 100 140 180 220))))))

(defn part-2
  "Day 10 Part 2"
  [input]
  (let [xvals (map last (part-1 input 240))]
    (str/join "\n" (map (fn [row] (str/join "" (map-indexed #(if (#{(inc %2) (dec %2) %2} %1) "#" ".") row))) (partition-all 40 xvals)))))

