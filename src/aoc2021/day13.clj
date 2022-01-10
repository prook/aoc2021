(ns aoc2021.day13
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def example (slurp "input/day13.example"))
(def input (slurp "input/day13"))

(defn parse
  [input]
  (letfn [(parse-dot [dot]
            (->> (str/split dot #",")
                 ((juxt (comp parse-long first)
                        (comp parse-long second)))))
          (parse-fold [fold]
            (let [[_ axis offset] (re-matches #"^fold along ([xy])=(\d+)$" fold)]
              [(keyword axis)
               (parse-long offset)]))]

    (let [[dots folds] (->> (str/split-lines input)
                            (split-with (complement str/blank?)))]
      [(->> dots
            (map parse-dot)
            (into #{}))
       (->> folds
            (remove str/blank?)
            (map parse-fold))])))

(defn dots->gfx
  [dots]
  (let [maxx (inc (transduce (map first) max 0 dots))
        maxy (inc (transduce (map second) max 0 dots))]
    (->> (for [y (range maxy)]
           (apply str (for [x (range maxx)]
                        (if (contains? dots [x y])
                          "##" "  "))))
         (interpose "\n")
         (apply str))))

(defn fold-number
  [n offset]
  (if (<= n offset)
    n
    (- (* 2 offset) n)))

(def axis->idx
  {:x 0
   :y 1})

(defn fold-dot
  [dot [axis offset]]
  (update dot (axis->idx axis) fold-number offset))

(defn fold
  [dots fold]
  (into #{}
        (map #(fold-dot % fold))
        dots))

(defn part1
  ([] (part1 input))
  ([input]
   (let [[dots [f & _]] (parse input)]
     (count (fold dots f)))))

(defn part2
  ([] (part2 input))
  ([input]
   (let [[dots folds] (parse input)]
     (->> folds
          (reduce fold dots)
          (dots->gfx)))))
