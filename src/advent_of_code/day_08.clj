(ns advent-of-code.day-08
  (:require [advent-of-code.util :refer [make-grid grid-get]]))

(defn get-forest [input] (make-grid #"" #(Integer/parseInt %) input))
(defn zip [a b] (map vector a b))
(defn get-sightlines [x y width height] [(zip (reverse (range 0 x)) (repeat y))
                            (zip (range (inc x) width) (repeat y))
                            (zip (repeat x) (reverse (range 0 y)))
                            (zip (repeat x) (range (inc y) height))])
(defn part-1
  "Day 08 Part 1"
  [input]
  (let [forest (get-forest input)
        width (count (first forest))
        height (count forest)
        trimmed-coords (for [x (range 1 (dec width)) y (range 1 (dec height))] [x y])
        trimmed-trees (filter
                       (fn [[x y]] (let [sightlines (get-sightlines x y width height)
                                         this-height (grid-get forest x y)
                                         ]
                                     (some #(every? (fn [[sx sy]] (< (grid-get forest sx sy) this-height)) %) sightlines)))
                       trimmed-coords)
        ]
    (+ width width height height (- 4) (count trimmed-trees))
    ))

(defn part-2
  "Day 08 Part 2"
  [input]
  (let [forest (get-forest input)
        width (count (first forest))
        height (count forest)
        coords (for [x (range 1 (dec width)) y (range 1 (dec height))] [x y])
        scores (map (fn [[x y]]
                      (reduce * (map
                                 (fn [sightline] (let [[visible hidden] (split-with
                                                                         (fn [[sx sy]] (< (grid-get forest sx sy) (grid-get forest x y)))
                                                                         sightline)
                                                       score (+ (count visible) (if (not-empty hidden) 1 0))
                                                       ] score))
                                 (get-sightlines x y width height))))
                    coords)
        ]
    (apply max scores)
    ))
