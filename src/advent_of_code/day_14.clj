(ns advent-of-code.day-14 
  (:require [advent-of-code.util :refer [gen-grid grid-get grid-set neighbors
                                         points-in-range print-grid supersplit
                                         tap]]))

(defn is-wall? "is there a wall at x y?" ([walls x y]
  (some 
   (fn [wall-group]
     (some 
      (fn [[[wx1 wy1] [wx2 wy2]]] 
        (or (and (<= wx2 x wx1) (<= wy2 y wy1))
            (and (<= wx1 x wx2) (<= wy1 y wy2))))
      (partition 2 1 wall-group))) 
   walls)))
(defn print-map [grid] (print-grid grid #(condp = (grid-get grid %1 %2) -1 "#" 1 "o" 0 ".")))
(defn simulate 
  ([start-sand height start-grid] (simulate start-sand height neighbors grid-set grid-get start-grid))
  ([start-sand height nbr-fn set-fn get-fn start-grid]
  (let [new-sand (reduce
                  (fn [[sandx sandy] _next-y]
                    (when (some? sandx)
                      (let [[_ _ _ br b bl _ _] (nbr-fn start-grid sandx sandy)]
                        (cond
                          (= (get-fn start-grid [sandx sandy]) 1) nil
                          (some nil? [br b bl]) nil
                          (zero? b) [sandx (inc sandy)]
                          (zero? bl) [(dec sandx) (inc sandy)]
                          (zero? br) [(inc sandx) (inc sandy)]
                          :else [sandx sandy]))))
                  start-sand
                  (range height))] 
  (when (some? new-sand) (set-fn start-grid new-sand 1)))))
  
(defn part-1
  "Day 14 Part 1"
  [input]
  (let [parsed (supersplit [#"\n" #" -> " #","] #(Integer/parseInt %) input)
        all-points (mapcat identity parsed)
        [min-x max-x] [(apply min (map first all-points)) (apply max (map first all-points))]
        [min-y max-y] [0 (apply max (map last all-points))]
        [width height] [(inc (- max-x min-x)) (inc (- max-y min-y))]
        grid (gen-grid width height #(if (is-wall? parsed (+ %1 min-x) (+ %2 min-y)) -1 0))]
    (->> (repeat [(- 500 min-x) 0])
         (reductions
          (fn [last-grid sand]
            (simulate sand height last-grid)) 
          grid)
         (take-while some?)
         (count)
         (dec))))

(defn nbr-map [floor gridmap x y] 
  (let [inc-y (inc y)
        dec-y (dec y)
        inc-x (inc x)
        dec-x (dec x)
        dirs [[x dec-y] [inc-x dec-y] [inc-x y] [inc-x inc-y] [x inc-y] [dec-x inc-y] [dec-x y] [dec-x dec-y]]]
    (map #(cond (= (last %) floor) -1 (contains? gridmap %) (gridmap %) :else 0) dirs)))

;; experiment with materialized map was terrible, just use a hashmap
(defn part-2
  "Day 14 Part 2"
  [input]
  (let [parsed (supersplit [#"\n" #" -> " #","] #(Integer/parseInt %) input)
        all-points (mapcat identity parsed)
        [min-y max-y] [0 (+ 2 (apply max (map last all-points)))]
        height (- max-y min-y)
        start-map (into 
                   (hash-map) 
                   (mapcat 
                    (fn [group] (mapcat (fn [[p1 p2]] (map #(vector % -1) (points-in-range p1 p2))) 
                                     (partition 2 1 group))) 
                    parsed))]
    (->> (repeat [500 0])
         (reductions
          (fn [last-grid sand]
            ;;(print-grid 30 height #(if (last-grid [(+ 491 %1) %2]) (condp = (last-grid [(+ 491 %1) %2]) 1 "o" -1 "#") "."))
            (simulate sand height (partial nbr-map height) assoc #(last-grid %2) last-grid))
          start-map)
         (take-while some?)
         (count)
         (dec))))
