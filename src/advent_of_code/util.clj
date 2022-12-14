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

(defn gen-grid
  [width height initial-fn] (vec (map (fn [y] (map #(vec (initial-fn % y)) (range width))) (range height))))

(defn grid-get
  [grid x y] (get (get grid y) x))

(defn clamp
  ([v min max clamped] (cond (> v max) clamped (< v min) clamped :else v))
  ([v min max] (cond (> v max) max (< v min) min :else v))
  ([v [min max]] (clamp v min max)))

(defn tlbr [grid x y]
  (let [width (count (first grid))
        height (count grid)]
    [
     (grid-get grid x (clamp (dec y) 0 height nil))
     (grid-get grid (clamp (dec x) 0 width nil) y)
     (grid-get grid x (clamp (inc y) 0 height nil))
     (grid-get grid (clamp (inc x) 0 width nil) y)
     ]))