(ns aoc2021.day4
  (:require [clojure.string :as str]))

(def example
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

(def input (slurp "input/day4"))

(defn parse-drawn-numbers
  [line]
  (mapv parse-long (str/split line #",")))

(defn parse-board
  [lines]
  (let [parse-board-line (fn parse-board-line
                           [line]
                           (map parse-long (str/split (str/trim line) #"\s+")))]
    (map vector (mapcat parse-board-line lines) (repeat false))))

(defn parse-input
  [input]
  (let [lines (str/split-lines input)
        [drawn-numbers-line & board-lines] lines
        drawn-numbers (parse-drawn-numbers drawn-numbers-line)
        boards (->> board-lines
                    (remove str/blank?)
                    (partition 5)
                    (mapv (juxt parse-board (constantly nil))))]
    [drawn-numbers boards]))

(defn bingo?
  [board]
  (let [marks (map second board)
        bingo-somewhere? (fn [cols-or-rows]
                           (->> cols-or-rows
                                (map #(every? true? %))
                                (some true?)
                                (boolean)))
        on-a-row? (->> marks
                       (partition 5)
                       (bingo-somewhere?))
        in-a-column? (->> marks
                          (iterate rest)
                          (take 5)
                          (map #(take-nth 5 %))
                          (bingo-somewhere?))]
    (or on-a-row? in-a-column?)))

(defn board-score
  [board]
  (->> board
       (keep (fn [[n mark?]] (when-not mark? n)))
       (reduce +)))

(defn mark-number
  [[board state _] drawn-number]
  (let [board (map (fn [[board-number mark?]]
                     [board-number (or mark? (= board-number drawn-number))])
                   board)]
    [board (when (bingo? board)
             (* drawn-number (board-score board)))]))

(defn draw-seq
  [drawn-numbers boards]
  (when drawn-numbers
    (lazy-seq
      (let [[drawn-number & drawn-numbers] drawn-numbers
            boards (->> boards
                        (remove second)
                        (map #(mark-number % drawn-number)))]
        (conj (draw-seq drawn-numbers boards) boards)))))

(defn part1
  ([] (part1 input))
  ([input]
   (let [[drawn-numbers boards] (parse-input input)]
     (->> (draw-seq drawn-numbers boards)
          (mapcat concat)
          (filter second)
          (first)
          (second)))))

(defn part2
  ([] (part2 input))
  ([input]
   (let [[drawn-numbers boards] (parse-input input)]
     (->> (draw-seq drawn-numbers boards)
          (mapcat concat)
          (filter second)
          (last)
          (second)))))
