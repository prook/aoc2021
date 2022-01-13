(ns aoc2021.day17
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def example "target area: x=20..30, y=-10..-5")
(def input "target area: x=209..238, y=-86..-59")

(defn parse
  [input]
  (let [[_ l r b t] (re-matches #"target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)" input)]
    {:l (parse-long l)
     :r (parse-long r)
     :t (parse-long t)
     :b (parse-long b)}))

(defn step
  [{:keys [vx vy x y]}]
  {:x  (+ x vx)
   :y  (+ y vy)
   :vx (max 0 (dec vx))
   :vy (dec vy)})

(defn hit-possible?
  [{:keys [b r]}]
  (fn hit-possible?*
    [{:keys [x y]}]
    (and (<= x r)
         (>= y b))))

(defn hit?
  [{:keys [l r t b]}]
  (fn hit?*
    [{:keys [x y]}]
    (and (<= l x r)
         (>= t y b))))

(defn make-trajectory
  [box vx0 vy0]
  (->> (iterate step {:x 0 :y 0 :vx vx0 :vy vy0})
       (take-while (hit-possible? box))))

(defn max-y
  [trajectory]
  (loop [trajectory trajectory max-y Long/MIN_VALUE]
    (let [{:keys [y] :as pos} (first trajectory)]
      (cond
        (nil? pos) max-y
        (< y max-y) max-y
        :else (recur (next trajectory) (long y))))))

(defn hits?
  [box trajectory]
  (->> trajectory
       (filter (hit? box))
       (first)
       (some?)))

(defn all-hitting-trajectories
  [{:keys [l r b] :as box}]
  (let [low-vx0 (-> l (* 8)                                 ;; slower than this and
                    (inc) (Math/sqrt)                       ;; the probe falls short its target;
                    (/ 2) (long))                           ;; darn the wicked draft
        high-vx0 (inc r)
        low-vy0 (dec b)
        high-vy0 (- low-vy0)]
    (for [vx0 (range low-vx0 high-vx0)
          vy0 (range low-vy0 high-vy0)
          :let [trajectory (make-trajectory box vx0 vy0)]
          :when (hits? box trajectory)]
      trajectory)))

(defn part1
  ([] (part1 input))
  ([input]
   (->> (parse input)
        (all-hitting-trajectories)
        (transduce (map max-y) max Long/MIN_VALUE))))

(defn part2
  ([] (part2 input))
  ([input]
   (->> (parse input)
        (all-hitting-trajectories)
        (count))))
