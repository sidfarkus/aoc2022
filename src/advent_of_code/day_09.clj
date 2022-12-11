(ns advent-of-code.day-09
  (:require [advent-of-code.util :refer [supersplit]]))

(defn norm [v] (vec (map #(if (> % 1) 1 (if (< % -1) -1 %)) v)))
(defn dirmap [dir] ((hash-map "R", [1 0] "L", [-1 0] "U", [0 -1] "D", [0 1]) dir))
(defn simulate [start head-moves]
  (reduce (fn [m move]
            (let [head (map + (m :head) move)
                  tail (first (m :tail))
                  distance (map - head tail)
                  new-tail (if (and (<= (Math/abs (first distance)) 1) (<= (Math/abs (last distance) ) 1)) tail
                               (map + (norm distance) tail))
                  ]
              (hash-map
               :head head,
               :tail (conj (m :tail) new-tail),
               :tail-moves (conj (m :tail-moves) (if (not= new-tail tail) (norm distance) [0 0])))
              ))
          (hash-map :head start, :tail (list start), :tail-moves [])
          head-moves)
  )
(defn part-1
  "Day 09 Part 1"
  ([input] (count (distinct ((part-1 input 1) :tail))))
  ([input length]
  (let [initial-moves (->> (supersplit [#"\n" #" "] input)
                           (map (fn [[dir amount]] (take (Integer/parseInt amount) (repeat (dirmap dir)))))
                           (mapcat identity))]
       (reduce (fn [m _] (simulate [0 0] (m :tail-moves))) (simulate [0 0] initial-moves) (range (dec length)))
       )))

(defn part-2
  "Day 09 Part 2"
  [input]
  (count (distinct ((part-1 input 9) :tail))))

