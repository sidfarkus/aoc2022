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

(defn zip [[h1 & rest1] [h2 & rest2]] 
  (if (= h1 h2 nil) [] (concat [[h1 h2]] (zip rest1 rest2))))

(defn make-grid
  ([column-delim str] (make-grid column-delim identity str))
  ([column-delim fn str] (vec (map vec (supersplit [#"\n" column-delim] fn str)))))

(defn gen-grid
  [width height initial-fn] (vec (map (fn [y] (map #(vec (initial-fn % y)) (range width))) (range height))))

(defn grid-get
  ([grid [x y]] (grid-get grid x y))
  ([grid x y] (get (get grid y) x)))

(defn map-grid [fun grid] 
  (let [width (count (first grid))
        height (count grid)]
    (apply concat (map (fn [y] (map #(fun % y) (range width))) (range height)))))

(defn some-grid [matchfn grid]
  (filter some? (map-grid matchfn grid)))

(defn print-grid [grid printfn]
  (let [gridstr (for [y (range (count grid))]
                      (cs/join "" (for [x (range (count (first grid)))]
                        (printfn x y))))] 
    (println (cs/join "\n" gridstr))))

(defn clamp
  ([v min max clamped] (cond (> v max) clamped (< v min) clamped :else v))
  ([v min max] (cond (> v max) max (< v min) min :else v))
  ([v [min max]] (clamp v min max)))

(defn tlbr [grid x y fun]
  (let [width (count (first grid))
        height (count grid)
        t [x (clamp (dec y) 0 height nil)]
        l [(clamp (dec x) 0 width nil) y]
        b [x (clamp (inc y) 0 height nil)]
        r [(clamp (inc x) 0 width nil) y]]
    [
     (fun (grid-get grid t) t)
     (fun (grid-get grid l) l)
     (fun (grid-get grid b) b)
     (fun (grid-get grid r) r)
     ]))