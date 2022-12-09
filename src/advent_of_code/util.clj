(ns advent-of-code.util
  (:require [clojure.string :as cs]))

(defn supersplit [delimiters str]
  (if
    (not-empty delimiters) (map (partial supersplit (rest delimiters)) (cs/split str (first delimiters)))
    str))

(defn make-2d [column-delim str] (vec (map vec (supersplit [#"\n" column-delim] str))))