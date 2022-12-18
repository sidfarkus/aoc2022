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

(defn first-arg [x _] x)
(defn second-arg [_ y] y)
(defn tap [fn & items] (apply fn (drop-last items)) (last items))
(defn zip [[h1 & rest1] [h2 & rest2]] 
  (if (= h1 h2 nil) [] (concat [[h1 h2]] (zip rest1 rest2))))

(defn int-vector-add [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])
(defn int-vector-diff [[x1 y1] [x2 y2]] (mapv #(cond (neg? %) -1 (zero? %) 0 :else 1) [(- x1 x2) (- y1 y2)]))
(defn points-in-range [p1 p2] 
  (conj (vec (take-while #(not= % p2) (iterate (partial int-vector-add (int-vector-diff p2 p1)) p1))) p2))

(defn make-grid
  ([column-delim str] (make-grid column-delim identity str))
  ([column-delim fn str] (mapv vec (supersplit [#"\n" column-delim] fn str))))

(defn gen-grid
  [width height initial-fn] (mapv (fn [y] (mapv #(initial-fn % y) (range width))) (range height)))

(defn grid-get
  ([grid [x y]] (grid-get grid x y))
  ([grid x y] (get (get grid y) x)))

(defn grid-set
  ([grid x y val] (assoc grid y (assoc (grid y) x val)))
  ([grid [x y] val] (grid-set grid x y val)))

(defn map-grid [fun grid] 
  (let [width (count (first grid))
        height (count grid)]
    (apply concat (map (fn [y] (map #(fun % y) (range width))) (range height)))))

(defn some-grid [matchfn grid]
  (filter some? (map-grid matchfn grid)))

(defn print-grid 
  ([grid printfn] (print-grid (count (first grid)) (count grid) printfn))
  ([width height printfn]
  (let [gridstr (for [y (range height)]
                      (cs/join "" (for [x (range width)]
                        (printfn x y))))] 
    (println (cs/join "\n" gridstr)))))

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
(defn neighbors 
  "[top top-right right bot-right bottom bot-left left top-left]"
  ([grid x y fun]
  (let [width (count (first grid))
        height (count grid)
        inc-y (clamp (inc y) 0 height nil)
        dec-y (clamp (dec y) 0 height nil)
        inc-x (clamp (inc x) 0 width nil)
        dec-x (clamp (dec x) 0 width nil)
        dirs [[x dec-y] [inc-x dec-y] [inc-x y] [inc-x inc-y] [x inc-y] [dec-x inc-y] [dec-x y] [dec-x dec-y]]]
    (map #(fun (grid-get grid %1) %1) dirs)))
  ([grid x y] (neighbors grid x y first-arg)))