(ns aoc2021.day12
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def example1 "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end\n")
(def example2 "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc")
(def example3 "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW\n")
(def input "DA-xn\nKD-ut\ngx-ll\ndj-PW\nxn-dj\nll-ut\nxn-gx\ndg-ak\nDA-start\nut-gx\nYM-ll\ndj-DA\nll-xn\ndj-YM\nstart-PW\ndj-start\nPW-gx\nYM-gx\nxn-ak\nPW-ak\nxn-PW\nYM-end\nend-ll\nak-end\nak-DA\n")

(defn small? [cave] (boolean (re-matches #"^[a-z]*$" cave)))

(defn parse
  [input]
  (->> (str/split-lines input)
       (map #(str/split % #"-"))
       (reduce (fn [acc [a b]]
                 (cond-> acc
                         (not= b "start") (update a (fnil conj #{}) b)
                         (not= a "start") (update b (fnil conj #{}) a)))
               {})))

(defn exits1
  "Each small cave can be visited at most once."
  [caves path]
  (reduce disj
          (caves (peek path))
          (->> path (filter small?))))

(defn exits2
  "One small cave can be visited twice."
  [caves path]
  (let [exits (caves (peek path))
        visited (->> path
                     (filter small?)
                     (frequencies))]
    (if (every? #{1} (vals visited))
      exits
      (remove visited exits))))

(defn paths
  ([exits caves] (paths exits caves ["start"]))
  ([exits caves path]
   (if (= "end" (peek path))
     [path]
     (->> path
          (exits caves)
          (map (partial conj path))
          (mapcat #(paths exits caves %))))))

(defn part1
  ([] (part1 input))
  ([input]
   (->> (parse input)
        (paths exits1)
        (count))))

(defn part2
  ([] (part2 input))
  ([input]
   (->> (parse input)
        (paths exits2)
        (count))))

