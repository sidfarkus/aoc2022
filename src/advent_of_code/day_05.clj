(ns advent-of-code.day-05
  (:require [clojure.string :as str]))

(defn next-move [popfn crates [num from to]]
  (let [popped (popfn (take num (get crates (dec from))))
        removed (last (split-at num (get crates (dec from))))
        added (concat popped (get crates (dec to)))]
    (assoc (assoc crates (dec to) (vec added)) (dec from) (vec removed)))
  )
(defn part-1
  "Day 05 Part 1"
  ([input popfn]
  (let [lines (apply vector (str/split-lines input))
        cratesLines (take-while #(not (re-matches #" 1.*" %)) lines)
        moveLines (map (fn [line] (map read-string (remove empty? (str/split line #" ?[a-z]+ ")))) (subvec lines (+ 2 (count cratesLines))))
        stackCount (read-string (last (str/split (get lines (count cratesLines)) #"\s+")))
        startCrates (apply mapv #(vec (remove empty? %&))
                           (map (fn [line]
                                  (vec (map #(str/replace (str/join "" %) #"[\[\] ]" "")
                                              (first (partition stackCount stackCount (repeat "") (partition-all 4 (char-array line)))))))
                                cratesLines))
        ]
    (str/join "" (map first (reduce (partial next-move popfn) startCrates moveLines)))))
  ([input] (part-1 input reverse)))

(defn part-2
  "Day 05 Part 2"
  [input]
  (part-1 input identity))
