(ns aoc2021.day10
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)

(def example (slurp "input/day10.example"))
(def input (slurp "input/day10"))

(defn parse
  [input]
  (->> input
       (str/split-lines)
       (map seq)))

(def paren-pairs
  {\( \)
   \[ \]
   \{ \}
   \< \>})

(def error-scores
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(def autocomplete-scores
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn check-line
  [l]
  (loop [[ch & chs] l
         expect []]
    (cond
      (not ch) [:ok (reverse expect)]

      (contains? paren-pairs ch)
      (recur chs (conj expect (get paren-pairs ch)))

      (= ch (peek expect))
      (recur chs (pop expect))

      :else
      [:error ch])))

(defn autocomplete-score
  [parens]
  (->> parens
       (map autocomplete-scores)
       (reduce (fn [acc score]
                 (-> acc
                     (* 5)
                     (+ score)))
               0)))

(defn part1
  ([] (part1 input))
  ([input]
   (->> (parse input)
        (map check-line)
        (filter (comp #{:error} first))
        (map (comp error-scores second))
        (reduce +))))

(defn part2
  ([] (part2 input))
  ([input]
   (->> (parse input)
        (map check-line)
        (filter (comp #{:ok} first))
        (map second)
        (map autocomplete-score)
        (sort)
        ((juxt (comp #(quot % 2) count) identity))
        (apply drop)
        (first))))
