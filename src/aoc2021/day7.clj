(ns aoc2021.day7
  (:require [clojure.string :as str]))

(def example "16,1,2,0,4,2,7,1,2,14")
(def input (slurp "input/day7"))

(defn parse
  [input]
  (->> (str/split (str/trim input) #",")
       (map parse-long)))

(defn fuel-requirement
  [consumption-fn crabs ^long to]
  (letfn [(fuel-consumption [^long from]
            (-> (- from to)
                (Math/abs)
                (consumption-fn)))]
    (->> crabs
         (map fuel-consumption)
         (reduce +))))

(def linear-consumption
  (->> (range)
       (take 2000)
       (vec)))

(def fib-consumption
  (->> (range)
       (map (fn [i] (quot (* i (inc i)) 2)))
       (take 2000)
       (vec)))

(defn min-consumption
  [input consumption]
  (let [crabs (parse input)
        min-destination (reduce min crabs)
        max-destination (reduce max crabs)
        fuel-requirements (map (partial fuel-requirement consumption crabs)
                               (range min-destination (inc max-destination)))]
    (reduce min fuel-requirements)))

(defn part1
  ([] (part1 input))
  ([input]
   (min-consumption input linear-consumption)))

(defn part2
  ([] (part2 input))
  ([input]
   (min-consumption input fib-consumption)))
