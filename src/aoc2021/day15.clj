(ns aoc2021.day15
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]])
  (:import (java.util Arrays)))

;;
;; this was a brutal one for me.
;;
;; at first, both the example and input data lead me to misunderstand
;; the problem, as solutions to part1 (both datasets) and part2 (example data)
;; were paths that only ever moved down or right, never up or left.
;;
;; after implementing logic that only moves D or R two different ways, and not
;; getting the correct result, I started looking at the possibility that U and
;; L motions could be the possibility. thus, the third implementation was born.
;; the only problem was it seemed to run forever for part2 with input data.
;; I was not patient enough to see if it ever finishes or is actually stuck
;; in an endless loop.
;;
;; as I'm walking through the AoC with tonsky's streams, I gave up and took
;; a look at his code, only to find out our solutions were almost identical.
;; the only difference was that I used a set for queue, while he used
;; a priority-map.
;;
;; at this point, I had no clue what a priority-map was, and I spent probably
;; 6 hours trying to understand the difference, even screaming "I don't get it,
;; what is this!!!" in my weaker moments. out of desperation, I ended up
;; printing the risks map after each step, which, at last, provided me with
;; the missing insight: my unprioritized queue caused the algorithm to wander
;; aimlessly around the data, overwriting large portions of the risks map over
;; and over again with only slightly better solutions. it would eventually
;; finish, but the process was criminally inefficient, possibly as inefficient
;; as bruteforcing the problem with random paths.
;;
;; contrary to that, solving the lower risk points first moves through the data
;; more methodically, breadth first, which means the algorithm never digs in a
;; too deep a dead end, doing throwaway work.
;;
;; lesson learned, I now have priority-map is now in my toolbox. also, I've
;; lost a day of my life.
;;

(set! *warn-on-reflection* true)

(def example (slurp "input/day15.example"))
(def input (slurp "input/day15"))

(defn parse-num [b] (- (long b) (long \0)))

(defn parse
  ([input] (parse input 1))
  ([input blowup-q]
   (let [in (str/split-lines input)
         orig-w (count (first in))
         orig-h (count in)
         w (* blowup-q orig-w)
         h (* blowup-q orig-h)
         out (make-array Long/TYPE w h)]
     (loop [rows (cycle in)
            y 0]
       (when (< y h)
         (loop [cs (cycle (first rows))
                x 0]
           (when (< x w)
             (aset out x y (-> (first cs)
                               (parse-num)
                               (+ (quot x orig-w)
                                  (quot y orig-h))
                               (dec)
                               (rem 9)
                               (inc)))
             (recur (next cs) (inc x))))
         (recur (next rows) (inc y))))
     {:width  w
      :height h
      :cave   out})))

(defn neighbours
  [max-x max-y [x y]]
  (cond-> []
          (> x 0) (conj [(dec x) y])
          (> y 0) (conj [x (dec y)])
          (< x max-x) (conj [(inc x) y])
          (< y max-y) (conj [x (inc y)])))

(defn init-risks!
  [width height]
  (let [risks (make-array Long/TYPE width height)]
    (doseq [x (range (count risks))]
      (Arrays/fill ^longs (aget ^"[[J" risks x) Long/MAX_VALUE))
    (aset risks 0 0 0)
    risks))

(defn minimize-risk
  [{:keys [width height cave]}]
  (let [risks (init-risks! width height)
        max-x (dec width)
        max-y (dec height)
        _exit [max-x max-y]
        neighbours (partial neighbours max-x max-y)]
    (loop [queue (priority-map [0 0] 0)]
      (when-some [[pos risk] (first queue)]
        (let [movement-risk (fn [[nx ny]]
                              (+ risk (aget cave nx ny)))
              less-risky? (fn [[[nx ny] nrisk]]
                            (< nrisk (aget risks nx ny)))
              ns (->> (neighbours pos)
                      (map (juxt identity movement-risk))
                      (filter less-risky?))]
          (doseq [[[nx ny] nrisk] ns]
            (aset risks nx ny nrisk))
          (recur (into (dissoc queue pos) ns)))))
    (aget risks max-x max-y)))

(defn part1
  ([] (part1 input))
  ([input]
   (-> input
       (parse)
       (minimize-risk))))

(defn part2
  ([] (part2 input))
  ([input]
   (-> input
       (parse 5)
       (minimize-risk))))
