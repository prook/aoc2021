(ns aoc2021.day3
  (:require [clojure.java.io :as io]))

(defn char->bit [c] (- (byte c) 48))
(defn string->bits [s] (mapv char->bit s))
(def input (with-open [r (io/reader "input/day3")]
             (->> (line-seq r)
                  (map string->bits)
                  (vec))))

(defn count-ones
  [input]
  (reduce (fn [acc row]
            (mapv + acc row))
          input))

(defn most-common-bits
  [input]
  (let [input-size (count input)
        quora (/ input-size 2)
        one-counts (count-ones input)]
    (mapv (fn [one-count]
            (if (> one-count quora)
              1
              0))
          one-counts)))

(defn bits->long
  [bits]
  (reduce (fn [acc bit]
            (+ (bit-shift-left acc 1)
               bit))
          0
          bits))

(defn part1
  []
  (let [mcb (most-common-bits input)
        gamma-rate (bits->long mcb)
        epsilon-rate (bits->long (map #(- 1 %) mcb))]
    (* gamma-rate epsilon-rate)))

(defn most-common-or-1
  [n input]
  (let [c (count input)
        ones (->> input
                  (map #(nth % n))
                  (reduce +))
        zeros (- c ones)]
    (if (>= ones zeros) 1 0)))

(defn least-common-or-0
  [n input]
  (- 1 (most-common-or-1 n input)))

(defn search
  ([input f] (search 0 input f))
  ([n input f]
   (let [expect (f n input)
         criteria (fn [row] (= (nth row n) expect))
         r (filter criteria input)]
     (cond
       (next r) (search (inc n) r f)
       :else (first r)))))

(defn part2
  []
  (let [oxygen-gen-rating (bits->long (search input most-common-or-1))
        co2-scrubber-rating (bits->long (search input least-common-or-0))]
    (* oxygen-gen-rating co2-scrubber-rating)))
