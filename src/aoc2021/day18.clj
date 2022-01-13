(ns aoc2021.day18
  (:require [clojure.string :as str]
            [aoc2021.util :refer [cond+]]
            [clojure.zip :as z]))

(set! *warn-on-reflection* true)

(def input (slurp "input/day18"))

(defn parse
  [input]
  (->> (str/split-lines input)
       (map read-string)))

(defn magnitude
  [num]
  (if (vector? num)
    (+ (* 3 (magnitude (first num)))
       (* 2 (magnitude (second num))))
    num))

(defn -add-in-dir
  [forward back stop? orig-loc n]
  (loop [loc (forward orig-loc) steps 1]
    (cond
      (stop? loc) orig-loc

      (number? (z/node loc))
      (nth (iterate back (z/edit loc + n)) steps)

      :else
      (recur (forward loc) (inc steps)))))

(def add-left (partial -add-in-dir z/prev z/next nil?))
(def add-right (partial -add-in-dir z/next z/prev z/end?))

(defn explode
  [num]
  (letfn [(explodes? [loc]
            (and (z/branch? loc)
                 (>= (count (z/path loc)) 4)
                 (every? number? (z/children loc))))]
    (loop [loc (z/vector-zip num)]
      (cond
        (z/end? loc) [:done (z/root loc)]
        (explodes? loc) [:changed
                         (let [[l r] (z/node loc)]
                           (-> loc
                               (z/replace 0)
                               (add-left l)
                               (add-right r)
                               (z/root)))]
        :else (recur (z/next loc))))))

(defn split
  [num]
  (letfn [(splits? [loc]
            (and (not (z/branch? loc))
                 (>= (z/node loc) 10)))
          (split [n] [(quot n 2) (quot (inc n) 2)])]
    (loop [loc (z/vector-zip num)]
      (cond
        (z/end? loc) [:done (z/root loc)]
        (splits? loc) [:chaged
                       (-> loc
                           (z/edit split)
                           (z/root))]
        :else (recur (z/next loc))))))

(defn snail-reduce
  [num]
  (loop [num num]
    (cond+
      :let [[status num] (explode num)]
      (not= :done status) (recur num)

      :let [[status num] (split num)]
      (not= :done status) (recur num)

      :else
      num)))

(defn snail-add
  ([a] (snail-reduce a))
  ([a b] (snail-reduce [a b])))

(defn part1
  ([] (part1 input))
  ([input]
   (->> (parse input)
        (reduce snail-add)
        (magnitude))))

(defn part2
  ([] (part2 input))
  ([input]
   (let [ns (parse input)]
     (reduce max
             Long/MIN_VALUE
             (for [a ns
                   b ns]
               (magnitude (snail-add a b)))))))


