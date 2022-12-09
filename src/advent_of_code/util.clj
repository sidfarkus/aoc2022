(ns advent-of-code.util
  (:require [clojure.string :as cs]))

(defn supersplit
  ([delimiters to-split]
   (supersplit delimiters identity to-split))
  ([delimiters fn to-split]
   (if
    (not-empty delimiters)
     (map (partial supersplit (rest delimiters) fn) (cs/split to-split (first delimiters)))
    (fn to-split))))


(defn make-grid
  ([column-delim str] (make-grid column-delim identity str))
  ([column-delim fn str] (vec (map vec (supersplit [#"\n" column-delim] fn str)))))

(defn grid-get
  [grid x y] (get (get grid y) x))