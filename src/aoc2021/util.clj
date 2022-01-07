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

