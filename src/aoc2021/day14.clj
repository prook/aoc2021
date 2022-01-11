(ns aoc2021.day14
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)

(def example (slurp "input/day14.example"))
(def input (slurp "input/day14"))

(defn parse
  [input]
  (let [parse-rule (fn [rule]
                     (let [[_ a b c] (re-matches #"^([A-Z])([A-Z]) -> ([A-Z])$" rule)]
                       [(str a b) [(str a c) (str c b)]]))
        lines (str/split-lines input)
        template (->> (first lines)
                      (partition-all 2 1)
                      (map #(apply str %))
                      (frequencies))
        rules (into {} (map parse-rule) (nnext lines))]
    [rules template]))

(defn add-pair
  [acc pair count]
  (update acc pair (fnil + 0) count))

(defn polymerize-fn
  [rules]
  (fn
    [template]
    (reduce-kv
      (fn [acc pair count]
        (if-let [[pair-a pair-b] (rules pair)]
          (-> acc
              (add-pair pair-a count)
              (add-pair pair-b count))
          (-> acc
              (add-pair pair count))))
      {}
      template)))

(defn polymer-fingerprint
  [polymer]
  (reduce-kv
    (fn [acc [a _] count]
      (update acc a (fnil + 0) count))
    {}
    polymer))

(defn polymer-strength
  [[rules template] num-steps]
  (let [polymerize (polymerize-fn rules)
        polymer (nth (iterate polymerize template) num-steps)
        fp (polymer-fingerprint polymer)
        fp-vals (vals fp)]
    (- (apply max fp-vals)
       (apply min fp-vals))))

(defn part1
  ([] (part1 input))
  ([input]
   (polymer-strength (parse input) 10)))


(defn part2
  ([] (part2 input))
  ([input]
   (polymer-strength (parse input) 40)))
