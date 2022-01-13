(ns aoc2021.util)

(defmacro
  results
  [label & forms]
  `(do
     (println ~label)
     ~@(loop [[f & forms] forms
              l []]
         (if-not f
           l
           (recur forms
                  (conj l `(println
                             ~(if forms "├" "└")
                             '~f "->" ~f)))))))

(defmacro cond+ [& clauses]
  (when-some [[test expr & rest] clauses]
    (case test
      :do   `(do ~expr (cond+ ~@rest))
      :let  `(let ~expr (cond+ ~@rest))
      :some `(or ~expr (cond+ ~@rest))
      `(if ~test ~expr (cond+ ~@rest)))))
