(ns aoc2021.day6
  (:require [clojure.string :as str]))

(def example (slurp "input/day6.example"))
(def input (slurp "input/day6"))

(defn parse
  [input]
  (->> (str/split (str/trim input) #",")
       (map parse-long)
       (frequencies)))

(defn next-day
  [today]
  (let [fishes (into {}
                     (map (fn [[age count]] [(dec age) count]))
                     today)
        num-spawns (get fishes -1 0)]
    (-> fishes
        (dissoc -1)
        (update 6 (fnil + 0) num-spawns)
        (update 8 (fnil + 0) num-spawns))))

(defn count-fishes
  [fishes]
  (->> fishes
       (map second)
       (reduce +)))

(defn count-fishes-after-days
  [fishes days]
  (->> (iterate next-day fishes)
       (drop days)
       (first)
       (count-fishes)))

(defn part1
  ([] (part1 input))
  ([input]
   (-> input
       (parse)
       (count-fishes-after-days 80))))

(defn part2
  ([] (part2 input))
  ([input]
   (-> input
       (parse)
       (count-fishes-after-days 256))))
