(ns advent-of-code.day-07
  (:require [advent-of-code.util :refer [supersplit]]))

(defn dir-sizes [size-map path dirs]
  (let [[_$ _cd dir] (first dirs)
        working-dir (conj path dir)
        ls-start (drop-while (fn [[_$ cmd _]] (= cmd "ls")) (rest dirs))
        contents (take-while (fn [[cmd _name _]] (not= cmd "$")) ls-start)
        sizes (reduce (fn [m [dir-or-size name]]
                        (if (= dir-or-size "dir")
                          m
                          (reduce (fn [m2 working-path]
                                    (update m2 working-path (fnil + 0) (Integer/parseInt dir-or-size)))
                                  m
                                  (take (count working-dir) (iterate drop-last working-dir)))))
                      size-map
                      contents)
        next-dirs (drop (count contents) ls-start)]

    (if (= dir "..")
      (dir-sizes size-map (vec (drop-last path)) (rest dirs))
      (if (not-empty next-dirs)
        (dir-sizes sizes working-dir next-dirs)
        sizes))))

(defn part-1
  "Day 07 Part 1"
  [input]
  (->> (supersplit [#"\n" #" "] input)
       (dir-sizes (hash-map) [])
       (vals)
       (filter (partial > 100000))
       (reduce +)))


(defn part-2
  "Day 07 Part 2"
  [input]
  (let [required-space 30000000
        total-space 70000000
        sizes (->> (supersplit [#"\n" #" "] input)
                   (dir-sizes (hash-map) []))
        root-size (get sizes ["/"])
        free-space (- total-space root-size)
        delta (- required-space free-space)
        ]
    (first (drop-while #(< % delta) (sort (vals sizes))))))