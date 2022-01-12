(ns aoc2021.day16
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)


(def input (slurp "input/day16"))

(def example0 "D2FE28")                                     ;; -> literal 2021

(def example1 "38006F45291200")                             ;; 27 bits of subpackets
(def example2 "EE00D40C823060")                             ;; 3 subpackets

(def example3 "8A004A801A8002F478")                         ;; -> sum ver 16
(def example4 "620080001611562C8802118E34")                 ;; -> sum ver 12
(def example5 "C0015000016115A2E0802F182340")               ;; -> sum ver 23
(def example6 "A0016C880162017C3686B18A3D4780")             ;; -> sum ver 31

(def -hex->bin
  {\0 [0 0 0 0]
   \1 [0 0 0 1]
   \2 [0 0 1 0]
   \3 [0 0 1 1]
   \4 [0 1 0 0]
   \5 [0 1 0 1]
   \6 [0 1 1 0]
   \7 [0 1 1 1]
   \8 [1 0 0 0]
   \9 [1 0 0 1]
   \A [1 0 1 0]
   \B [1 0 1 1]
   \C [1 1 0 0]
   \D [1 1 0 1]
   \E [1 1 1 0]
   \F [1 1 1 1]})

(def ->type-id
  {0 :sum
   1 :product
   2 :min
   3 :max
   4 :literal
   5 :gt
   6 :lt
   7 :eq})

(defn parse-hex [hex-str] (mapcat -hex->bin hex-str))

(defn ->num
  [bits]
  (reduce (fn [acc bit]
            (-> acc
                (bit-shift-left 1)
                (bit-or bit)))
          0
          bits))

(defn parse-version
  [bits]
  [(->> bits (take 3) (->num))
   (->> bits (drop 3))])

(defn parse-type-id
  [bits]
  [(->> bits (take 3) (->num) (->type-id))
   (->> bits (drop 3))])

(defn parse-literal
  [bits]
  (loop [literal-bits [] bits bits]
    (let [[more & literal-nibble] (take 5 bits)
          literal-bits (concat literal-bits literal-nibble)
          bits (drop 5 bits)]
      (if (zero? more)
        [{:value (->num literal-bits)} bits]
        (recur literal-bits bits)))))

(declare parse-packet)
(declare parse-packets)

(defn parse-subpackets-0
  [bits]
  (let [num-bits (->> bits (take 15) (->num))
        bits (drop 15 bits)
        packet-bits (take num-bits bits)
        bits (drop num-bits bits)
        [packets _bits] (parse-packets packet-bits)]
    [{:packet-bits num-bits
      :packets     packets}
     bits]))

(defn parse-subpackets-1
  [bits]
  (let [num-packets (->> bits (take 11) (->num))
        bits (drop 11 bits)
        [packets bits] (loop [packets [] bits bits i num-packets]
                         (let [[packet bits] (parse-packet bits)
                               packets (conj packets packet)
                               i (dec i)]
                           (if (> i 0)
                             (recur packets bits i)
                             [packets bits])))]
    [{:num-packets num-packets
      :packets     packets}
     bits]))

(defn parse-operator
  [bits]
  (let [[length-type & bits] bits]
    (case (long length-type)
      0 (parse-subpackets-0 bits)
      1 (parse-subpackets-1 bits)
      (throw (ex-info "TF?" {})))))

(defn operator? [type-id] (not= type-id :literal))
(defn literal? [type-id] (= type-id :literal))

(defmulti apply-operator (fn [type-id _payload] type-id))

(defmethod apply-operator :literal
  [_ payload] payload)

(defmethod apply-operator :sum
  [_ {:as payload :keys [packets]}]
  (assoc payload
    :value (reduce (fn [acc {:keys [value]}] (+ acc value)) 0 packets)))

(defmethod apply-operator :product
  [_ {:as payload :keys [packets]}]
  (assoc payload
    :value (reduce (fn [acc {:keys [value]}] (* acc value)) 1 packets)))

(defmethod apply-operator :min
  [_ {:as payload :keys [packets]}]
  (assoc payload
    :value (reduce (fn [acc {:keys [value]}] (min acc value)) Long/MAX_VALUE packets)))

(defmethod apply-operator :max
  [_ {:as payload :keys [packets]}]
  (assoc payload
    :value (reduce (fn [acc {:keys [value]}] (max acc value)) Long/MIN_VALUE packets)))

(defmethod apply-operator :gt
  [_ {:as payload :keys [packets]}]
  (let [[left right] packets]
    (assoc payload
      :value (if (> (:value left) (:value right)) 1 0))))

(defmethod apply-operator :lt
  [_ {:as payload :keys [packets]}]
  (let [[left right] packets]
    (assoc payload
      :value (if (< (:value left) (:value right)) 1 0))))

(defmethod apply-operator :eq
  [_ {:as payload :keys [packets]}]
  (let [[left right] packets]
    (assoc payload
      :value (if (= (:value left) (:value right)) 1 0))))

(defn parse-packet
  [bits]
  (let [[version bits] (parse-version bits)
        [type-id bits] (parse-type-id bits)
        [payload bits] (cond
                         (literal? type-id) (parse-literal bits)
                         (operator? type-id) (parse-operator bits))
        packet (merge (apply-operator type-id payload)
                      {:version version
                       :type-id type-id})]
    [packet bits]))

(defn parse-packets
  ([bits] (parse-packets [] bits))
  ([packets bits]
   (let [[packet bits] (parse-packet bits)
         packets (conj packets packet)]
     (if (or (empty? bits)
             (every? zero? bits))
       [packets bits]
       (parse-packets packets bits)))))

(defn sum-versions
  [packets]
  (reduce
    (fn [acc {:keys [version packets]}]
      (+ acc
         version
         (sum-versions packets)))
    0
    (seq packets)))

(defn part1
  ([] (part1 input))
  ([input]
   (->> input
        (parse-hex)
        (parse-packets)
        (first)
        (sum-versions))))

(defn part2
  ([] (part2 input))
  ([input]
   (->> input
        (parse-hex)
        (parse-packets)
        (ffirst)
        :value)))

(comment
  (->> example1
       (parse-hex)
       (parse-packet)))
