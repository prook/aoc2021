(ns aoc2021.day9
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)

(def example (slurp "input/day9.example"))
(def input (slurp "input/day9"))

(defn parse
  [input]
  (->> (str/split-lines input)
       (map (fn [line] (map (comp parse-long str) line)))))

(defn width [hmap] (count (first hmap)))
(defn height [hmap] (count hmap))
(defn val-at [hmap [x y]] (nth (nth hmap y) x))

(defn neighbors
  [hmap [x y]]
  (cond-> []
          (> x 0) (conj [(dec x) y])
          (< x (dec (width hmap))) (conj [(inc x) y])
          (> y 0) (conj [x (dec y)])
          (< y (dec (height hmap))) (conj [x (inc y)])))

(defn low-points
  [hmap]
  (for [x (range (width hmap))
        y (range (height hmap))
        :let [coords [x y]
              p (val-at hmap coords)]
        :when (every? #(< p (val-at hmap %)) (neighbors hmap coords))]
    coords))

(defn part1
  ([] (part1 input))
  ([input]
   (let [hmap (parse input)]
     (->> hmap
          (low-points)
          (map #(val-at hmap %))
          (map inc)
          (reduce + 0)))))

(defn part2
  ([] (part2 input))
  ([input]
   (let [hmap (parse input)
         fill-basin (fn fill-basin [start]
                      (loop [front [start] basin #{}]
                        (let [pos (peek front)]
                          (cond
                            (empty? front)
                            basin

                            (contains? basin pos)
                            (recur (pop front) basin)

                            (= 9 (val-at hmap pos))
                            (recur (pop front) basin)

                            :else
                            (recur (into front (neighbors hmap pos)) (conj basin pos))))))]
     (->> hmap
          (low-points)
          (map fill-basin)
          (distinct)
          (map count)
          (sort)
          (reverse)
          (take 3)
          (reduce *)))))
