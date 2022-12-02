(ns advent-of-code.day-02
  (:require [clojure.string :as str]))

(defn win-map [item] ((hash-map "A" "C", "B" "A", "C" "B") item))
(defn lose-map [item] ((hash-map "A" "B", "B" "C", "C" "A") item))
(defn translate [item] ((hash-map "X" "A", "Y" "B", "Z" "C") item))
(defn score-item-1 [first, second]
  (+ (if (= (win-map first) (translate second)) 0 (if (= first (translate second)) 3 6)) (.indexOf ["" "X" "Y" "Z"] second)))
(defn score-item-2 [theirs, mine]
  (let [choice (if (= mine "X") (win-map theirs) (if (= mine "Y") theirs (lose-map theirs)))]
    (+ (.indexOf ["" "A" "B" "C"] choice) ((hash-map "X" 0, "Y" 3, "Z" 6) mine))))
(defn score [scorefn, line] (apply scorefn (str/split line #" ")))

(defn part-1
  "Day 02 Part 1"
  [input]
  (reduce + (map (partial score score-item-1) (str/split input #"\n"))))

(defn part-2
  "Day 02 Part 2"
  [input]
  (reduce + (map (partial score score-item-2) (str/split input #"\n"))))
