(ns aoc2021.day1
  (:require [clojure.java.io :as io]
            [aoc2021.util :refer [results]]))

(defn part1
  []
  (let [xs (with-open [r (io/reader "input/day1")]
             (->> r
                  (line-seq)
                  (mapv parse-long)))]
    (->> (map - xs (next xs))
         (filter neg?)
         (count))))

(defn part2
  []
  (let [xs (with-open [r (io/reader "input/day1")]
             (->> r
                  (line-seq)
                  (map #(Long/parseLong %))
                  (partition 3 1)
                  (mapv #(apply + %))))]
    (->> (map - xs (next xs))
         (filter neg?)
         (count))))

(results "Day 1"
         (part1)
         (part2))
