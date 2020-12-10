(ns aoc.day10
  (:require [clojure.java.io :as io]))

(defn solve [numbers]
  (let [increments (map - numbers (conj numbers 0))
        [i1 i3] (map #(count (filter #{%} increments)) [1 3])]
    (* i1 (inc i3))))

(defn solve2 [numbers]
  ((reduce (fn [seen el]
              (conj seen
                    [el (apply + (map #(get seen % 0) (range (- el 3) el)))]))
            {0 1}
           numbers)
     (last numbers)))

(defn get-input []
  (with-open [rdr (io/reader "resources/input10.txt")]
    (mapv #(Integer/parseInt %) (line-seq rdr))))

(defn run []
  (let [numbers (sort (get-input))]
    (println (solve numbers))
    (println (solve2 numbers))))
