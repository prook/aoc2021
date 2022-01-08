(ns aoc2021.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line
  [s]
  (let [[m x] (str/split s #" ")]
    [(keyword m) (parse-long x)]))

(defn part1
  []
  (let [movements (with-open [r (io/reader "input/day2")]
                    (->> r
                         (line-seq)
                         (mapv parse-line)))
        {:keys [position depth]} (reduce (fn [acc [m x]]
                                           (cond-> acc
                                                   (= m :forward) (update :position + x)
                                                   (= m :up) (update :depth - x)
                                                   (= m :down) (update :depth + x)))
                                         {:position 0
                                          :depth    0}
                                         movements)]
    (* position depth)))

(defn forward
  [{:keys [aim position depth]} x]
  {:aim      aim
   :position (+ position x)
   :depth    (+ depth (* x aim))})

(defn part2
  []
  (let [movements (with-open [r (io/reader "input/day2")]
                    (->> r
                         (line-seq)
                         (mapv parse-line)))
        {:keys [position depth]} (reduce (fn [acc [m x]]
                                           (cond-> acc
                                                   (= m :up) (update :aim - x)
                                                   (= m :down) (update :aim + x)

                                                   (= m :forward) (forward x)))

                                         {:aim      0
                                          :position 0
                                          :depth    0}
                                         movements)]
    (* position depth)))
