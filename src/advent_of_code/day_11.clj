(ns advent-of-code.day-11
  (:require [clojure.string :as str]))

(defn op-fn [opstr arg] (cond
                         (= arg "old") #(* % %)
                         (= opstr "*") (partial * (Integer/parseInt arg))
                         :else (partial + (Integer/parseInt arg))))
(defn parse-monkey [monkey-str]
  (let [[id items op arg test truem falsem]
        (rest (re-matches #"(?s).* (\d):\n.+s: ([\d ,]+)\n.+= old ([*+]) (\w+)\n.+by (\d+)\n.+ue:.+(\d+)\n.+se:.+(\d+)" monkey-str))]
    [id {:id id, :items (vec (map #(Integer/parseInt %) (str/split items #", "))), :op (op-fn op arg) :test (Integer/parseInt test) :truem truem, :falsem falsem, :insp 0}]))

(defn thrown-to [decay monkey item]
  (let [new-worry (quot ((monkey :op) item) decay)]
    [new-worry (if (= (mod new-worry (monkey :test)) 0) (monkey :truem) (monkey :falsem))]))

(defn monkey-business [lcm decay monkeys round]
  (reduce-kv
   (fn [m monkey-id _]
     (let [this-monkey (m monkey-id)
           thrown-items (group-by last (map (partial thrown-to decay this-monkey) (this-monkey :items)))
           thrown-monkeys (mapcat (fn [[id worries]] [id (update (m id) :items #(apply conj (vec %) (map (fn [x] (mod x lcm)) (map first worries))))]) thrown-items)
           return-monkeys (apply assoc m
                                 monkey-id (assoc this-monkey :items [] :insp (+ (this-monkey :insp) (count (this-monkey :items))))
                                 thrown-monkeys)]
       return-monkeys))

   monkeys
   monkeys))

(defn part-1
  "Day 11 Part 1"
  ([input rounds decay]
   (let [start-monkeys (into (sorted-map) (map parse-monkey (str/split input #"\n\n")))
         monkey-lcm (reduce * (map :test (vals start-monkeys)))]
     (->> (reduce (partial monkey-business monkey-lcm decay) start-monkeys (range 1 (inc rounds)))
          (map #((last %) :insp))
          (sort >)
          (take 2)
          (reduce *))))
  ([input] (part-1 input 20 3)))

(defn part-2
  "Day 11 Part 2"
  [input]
  (part-1 input 10000 1))
