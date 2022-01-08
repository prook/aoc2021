(ns aoc2021.day5
  (:require [clojure.string :as str]))

(def example (slurp "input/day5.example"))
(def input (slurp "input/day5"))

(def line-pattern #"^(\d+),(\d+) -> (\d+),(\d+)$")

(defn parse-line
  [line]
  (let [[_ x1 y1 x2 y2] (re-matches line-pattern line)]
    [(parse-long x1)
     (parse-long y1)
     (parse-long x2)
     (parse-long y2)]))

(defn parse
  [input]
  (->> input
       (str/split-lines)
       (map parse-line)))

(defn horizontal? [[_ y1 _ y2]] (= y1 y2))
(defn vertical? [[x1 _ x2 _]] (= x1 x2))
(defn ortho? [lseg] (or (horizontal? lseg)
                        (vertical? lseg)))
(defn diagonal-bltr? [[x1 y1 x2 y2]] (and (< x1 x2) (< y1 y2)))
(defn diagonal-tlbr? [[x1 y1 x2 y2]] (and (< x1 x2) (> y1 y2)))
(defn diagonal-brtl? [[x1 y1 x2 y2]] (and (> x1 x2) (< y1 y2)))
(defn diagonal-trbl? [[x1 y1 x2 y2]] (and (> x1 x2) (> y1 y2)))

(defn bounding-box [lsegs] (let [[mx my] (reduce (fn [[x y] [x1 y1 x2 y2]] [(max x x1 x2) (max y y1 y2)]) lsegs)]
                             [(inc mx) (inc my)]))
(defn init-terrain [[x y]] (vec (repeat (* x y) 0)))

(defn make-offset
  [dim-x x y]
  (+ x (* dim-x y)))

(defn lseg->offsets
  [[dim-x _] [x1 y1 x2 y2 :as lseg]]
  (let [make-offset (partial make-offset dim-x)]
    (cond
      (horizontal? lseg) (map make-offset (range (min x1 x2) (inc (max x1 x2))) (repeat y1))
      (vertical? lseg) (map make-offset (repeat x1) (range (min y1 y2) (inc (max y1 y2))))
      (diagonal-bltr? lseg) (map make-offset (range x1 (inc x2)) (range y1 (inc y2)))
      (diagonal-tlbr? lseg) (map make-offset (range x1 (inc x2)) (reverse (range y2 (inc y1))))
      (diagonal-brtl? lseg) (map make-offset (reverse (range x2 (inc x1))) (range y1 (inc y2)))
      (diagonal-trbl? lseg) (map make-offset (reverse (range x2 (inc x1))) (reverse (range y2 (inc y1)))))))

(defn mark-terrain
  [terrain offset]
  (update terrain offset inc))

(defn part1
  ([] (part1 input))
  ([input]
   (let [lsegs (parse input)
         bb (bounding-box lsegs)
         terrain (init-terrain bb)]
     (->> lsegs
          (filter ortho?)
          (mapcat (partial lseg->offsets bb))
          (reduce mark-terrain terrain)
          (filter #(<= 2 %))
          (count)))))

(defn part2
  ([] (part2 input))
  ([input]
   (let [lsegs (parse input)
         bb (bounding-box lsegs)
         terrain (init-terrain bb)]
     (->> lsegs
          (mapcat (partial lseg->offsets bb))
          (reduce mark-terrain terrain)
          (filter #(<= 2 %))
          (count)))))
