(ns aoc2021.day8
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def example (slurp "input/day8.example"))
(def input (slurp "input/day8"))

(defn parse-line
  [l]
  (let [parse-signals (fn [ss]
                        (->> (str/split ss #"\s+")
                             (map set)))
        [signals output] (str/split l #"\s+\|\s+")]
    [(parse-signals signals)
     (parse-signals output)]))

(defn parse
  [input]
  (->> (str/split-lines input)
       (map parse-line)))

(def segments->digit
  {#{\a \b \c \e \f \g}    0
   #{\c \f}                1
   #{\a \c \d \e \g}       2
   #{\a \c \d \f \g}       3
   #{\b \c \d \f}          4
   #{\a \b \d \f \g}       5
   #{\a \b \d \e \f \g}    6
   #{\a \c \f}             7
   #{\a \b \c \d \e \f \g} 8
   #{\a \b \c \d \f \g}    9})

(defn scrambled-mapping
  [signals]
  (let [keep-count (fn [counts dwc]
                     (keep (fn [[digit-count digits]]
                             (when (contains? counts digit-count)
                               digits))
                           dwc))
        signals-with-counts (->> signals
                                 (map (juxt count identity)))

        digit-1 (->> signals-with-counts
                     (keep-count #{2})
                     (first))
        digit-8 (->> signals-with-counts
                     (keep-count #{7})
                     (first))
        digits-2345-segfreqs (->> signals-with-counts
                                  (keep-count #{4 5})
                                  (apply concat)
                                  (frequencies))

        segment-a (first (set/difference (first (keep-count #{3} signals-with-counts))
                                         (first (keep-count #{2} signals-with-counts))))
        segment-b (->> digits-2345-segfreqs
                       (keep (fn [[k v]]
                               (when (= v 2)
                                 k)))
                       (first))
        segment-d (->> digits-2345-segfreqs
                       (keep (fn [[k v]]
                               (when (= v 4)
                                 k)))
                       (first))
        segment-e (->> digits-2345-segfreqs
                       (keep (fn [[k v]]
                               (when (= v 1)
                                 k)))
                       (first))
        segment-f (->> signals-with-counts
                       (keep-count #{5})
                       (filter #(contains? % segment-e))
                       (first)
                       (set/difference digit-1)
                       (first))
        segment-c (-> digit-1
                      (disj segment-f)
                      (first))
        segment-g (-> digit-8
                      (disj segment-a
                            segment-b
                            segment-c
                            segment-d
                            segment-e
                            segment-f)
                      (first))]

    {segment-a \a
     segment-b \b
     segment-c \c
     segment-d \d
     segment-e \e
     segment-f \f
     segment-g \g}))

(defn unscramble-segments
  [unscramble-segment segments]
  (->> segments
       (map unscramble-segment)
       (set)))

(defn unscramble-output
  [scrambled-mapping scrambled-output]
  (->> scrambled-output
       (map (partial unscramble-segments scrambled-mapping))
       (map segments->digit)
       (reduce (fn [acc x]
                 (+ x (* acc 10)))
               0)))

(defn part1
  ([] (part1 input))
  ([input]
   (->> (parse input)
        (mapcat second)
        (map count)
        (filter #{2 3 4 7})
        (count))))

(defn part2
  ([] (part2 input))
  ([input]
   (let [data (parse input)]
     (->> data
          (map (fn [[signals output]]
                 (unscramble-output
                   (scrambled-mapping signals)
                   output)))
          (reduce +)))))
