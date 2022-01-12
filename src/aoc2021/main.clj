(ns aoc2021.main
  (:require [aoc2021.day1 :as d1]
            [aoc2021.day2 :as d2]
            [aoc2021.day3 :as d3]
            [aoc2021.day4 :as d4]
            [aoc2021.day5 :as d5]
            [aoc2021.day6 :as d6]
            [aoc2021.day7 :as d7]
            [aoc2021.day8 :as d8]
            [aoc2021.day9 :as d9]
            [aoc2021.day10 :as d10]
            [aoc2021.day11 :as d11]
            [aoc2021.day12 :as d12]
            [aoc2021.day13 :as d13]
            [aoc2021.day14 :as d14]
            [aoc2021.day15 :as d15]
            [aoc2021.day16 :as d16]
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
           (d5/part2))
  (results "Day 6"
           (d6/part1)
           (d6/part2))
  (results "Day 7"
           (d7/part1)
           (d7/part2))
  (results "Day 8"
           (d8/part1)
           (d8/part2))
  (results "Day 9"
           (d9/part1)
           (d9/part2))
  (results "Day 10"
           (d10/part1)
           (d10/part2))
  (results "Day 11"
           (d11/part1)
           (d11/part2))
  (results "Day 12"
           (d12/part1)
           (d12/part2))
  (results "Day 13"
           (d13/part1)
           (d13/part2))
  (results "Day 14"
           (d14/part1)
           (d14/part2))
  (results "Day 15"
           (d15/part1)
           (d15/part2))
  (results "Day 16"
           (d16/part1)
           (d16/part2)))
