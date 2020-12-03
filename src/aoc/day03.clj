(ns aoc.day03
  (:require [clojure.java.io :as io]))

(defn solve [trees [x-step y-step]]
  (count
    (filter identity
            (map (fn [row pos] (get row (rem pos (count row))))
                 (take-nth y-step trees)
                 (range 0 ##Inf x-step)))))

(defn solve2 [trees]
  (reduce
    *
    (map #(solve trees %)
         [[1 1] [3 1] [5 1] [7 1] [1 2]])))

(defn run []
  (with-open [rdr (io/reader "resources/input03.txt")]
    (let [trees (map #(into [] (map #{\#}) %) (line-seq rdr))]
      (println (solve trees [3 1]))
      (println (solve2 trees)))))
