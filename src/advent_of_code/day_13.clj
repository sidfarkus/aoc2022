(ns advent-of-code.day-13 
  (:require [advent-of-code.util :refer [supersplit zip]]
            [clojure.string :as str]))

(defn compare-items 
  ([[pair1 pair2]] (compare-items pair1 pair2))
  ([pair1 pair2]
  (cond
    (nil? pair1) -1
    (nil? pair2) 1
    (and (instance? Number pair1) (instance? Number pair2)) (compare pair1 pair2)
    (instance? Number pair1) (compare-items [[pair1] pair2])
    (instance? Number pair2) (compare-items [pair1 [pair2]])
    :else (map compare-items (zip pair1 pair2)))
  ))
(def pair-fn (comp first (partial filter (complement zero?)) flatten compare-items))

(defn part-1
  "Day 13 Part 1"
  [input]
  (let [pairs (supersplit [#"\n\n" #"\n"] input)
        lists (map #(map (fn [x] (read-string (str/replace x #"," " "))) %) pairs)
        ]
    (reduce + (map-indexed #(if (neg? %2) (inc %1) 0) (vec (map pair-fn lists))))))

(defn part-2
  "Day 13 Part 2"
  [input]
  (let [dividers #{[[2]] [[6]]}
        packets (concat dividers
                        (map #(read-string (str/replace % #"," " "))
                             (filter not-empty (str/split input #"\n"))))
        ]
    (reduce * (map-indexed #(if (contains? dividers %2) (inc %1) 1) (sort-by identity pair-fn packets)))
    ))
