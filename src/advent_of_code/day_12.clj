(ns advent-of-code.day-12 
  (:require [advent-of-code.util :refer [some-grid grid-get map-grid supersplit tlbr]]
            [astar.core :as astar]))

(defn reachable? [val other] 
  (and (some? other)
   (let [a (cond (= val "S") "a" (= val "E") "z" :else val)
         b (cond (= other "S") "a" (= other "E") "z" :else other)] 
       (contains? (set (map #(str (char %)) (range (inc (int (.charAt a 0))) 96 -1))) b))))
(defn dist [[x1 y1] [x2 y2]]
  (Math/abs (+ (- x1 x2) (- y1 y2))))
(defn part-1
  "Day 12 Part 1"
  ([input] (count (first (part-1 input #{"S"}))))
  ([input start-chars] 
  (let [grid (vec (map vec (supersplit [#"\n" #""] input)))
        starts (some-grid #(when (contains? start-chars (grid-get grid %1 %2)) [%1 %2]) grid)
        [end] (some-grid #(when (= (grid-get grid %1 %2) "E") [%1 %2]) grid)
        graph (apply hash-map
                     (apply concat (map-grid (fn [x y]
                         (let [val (grid-get grid x y)
                               edges (map second
                                          (filter #(reachable? val (first %))
                                                  (tlbr grid x y #(vector %1 %2))))]
                           [[x y] edges])) 
                        grid)))]
    (pmap (fn [start] (astar/route graph dist (partial dist end) start end)) starts))))

(defn part-2 
  "Day 12 Part 2"
  [input]
  (apply min (filter #(> % 0) (map count (part-1 input #{"S" "a"})))))
