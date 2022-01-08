(ns aoc2021.main
  (:require [aoc2021.day1 :as d1]
            [aoc2021.day2 :as d2]
            [aoc2021.day3 :as d3]
            [aoc2021.day4 :as d4]
            [aoc2021.day5 :as d5]
            [aoc2021.util :refer [results]]))


(defn -main [& _args]
  (results "Day 1"
           (d1/part1)
           (d1/part2))
  (results "Day 2"
           (d2/part1)
           (d2/part2))
  (results "Day 3"
           (d3/part1)
           (d3/part2))
  (results "Day 4"
           (d4/part1)
           (d4/part2))
  (results "Day 5"
           (d5/part1)
           (d5/part2)))
