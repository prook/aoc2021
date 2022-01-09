(ns aoc2021.day11
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)

(def example (slurp "input/day11.example"))
(def input (slurp "input/day11"))

(defn parse
  [input]
  (->> input
       (str/split-lines)
       (mapv #(mapv (comp parse-long str) %))))

(defn width [dumbos] (count (first dumbos)))
(defn height [dumbos] (count dumbos))
(defn val-at [dumbos [x y]] (-> dumbos (nth y) (nth x)))

(defn flash-area
  [dumbos [x y]]
  (let [max-x (dec (width dumbos))
        max-y (dec (height dumbos))]
    (cond-> [[x y]]
            ;; orthogonal directions
            (> x 0) (conj [(dec x) y])
            (< x max-x) (conj [(inc x) y])
            (> y 0) (conj [x (dec y)])
            (< y max-y) (conj [x (inc y)])
            ;; diagonals
            (and (> x 0) (> y 0)) (conj [(dec x) (dec y)])
            (and (> x 0) (< y max-y)) (conj [(dec x) (inc y)])
            (and (< x max-x) (> y 0)) (conj [(inc x) (dec y)])
            (and (< x max-x) (< y max-y)) (conj [(inc x) (inc y)]))))

(defn energize-at
  [dumbos [x y]]
  (update-in dumbos [y x] inc))

(defn flash-at
  [dumbos flash-point]
  (reduce energize-at dumbos (flash-area dumbos flash-point)))

(defn energize-all
  [dumbos]
  (mapv #(mapv inc %) dumbos))

(defn discharge-all
  [dumbos]
  (letfn [(discharge-one [dumbo]
            (if (>= dumbo 10) 0 dumbo))]
    (mapv #(mapv discharge-one %) dumbos)))

(defn detect-flashes
  [dumbos]
  (for [x (range (width dumbos))
        y (range (height dumbos))
        :let [coords [x y]]
        :when (= 10 (val-at dumbos coords))]
    coords))

(defn flashy-flashy
  [dumbos]
  (loop [dumbos dumbos
         flash-points (set (detect-flashes dumbos))
         flashed #{}]
    (if (empty? flash-points)
      dumbos
      (let [flash-point (first flash-points)
            dumbos (flash-at dumbos flash-point)
            flashed (conj flashed flash-point)
            flash-points (->> dumbos
                              (detect-flashes)
                              (into flash-points)
                              (remove flashed))]
        (recur dumbos
               flash-points
               flashed)))))

(defn count-flashes
  [dumbos]
  (->> dumbos
       (mapcat identity)
       (filter zero?)
       (count)))

(defn step
  [[dumbos _]]
  (let [next-dumbos (->> dumbos
                         (energize-all)
                         (flashy-flashy)
                         (discharge-all))]
    [next-dumbos (count-flashes next-dumbos)]))

(defn part1
  ([] (part1 input))
  ([input]
   (->> [(parse input) 0]
        (iterate step)
        (next)
        (take 100)
        (map second)
        (reduce +))))

(defn part2
  ([] (part2 input))
  ([input]
   (let [dumbos (parse input)
         num-dumbos (* (width dumbos) (height dumbos))]
     (->> [dumbos 0]
          (iterate step)
          (keep-indexed (fn [i [_ flashes]]
                          (when (= num-dumbos flashes)
                            i)))
          (first)))))
