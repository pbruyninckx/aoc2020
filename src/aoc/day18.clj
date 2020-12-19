(ns aoc.day18
  (:require [clojure.string :as str]))

(defn evaluate [expression]
  (if (number? expression)
    expression
    (reduce (fn [acc [op expr2]]
               ((eval op) acc (evaluate expr2)))
             (evaluate (first expression))
             (partition 2 (rest expression)))))

(defn solve [expressions]
  (->> expressions
       (map evaluate)
       (reduce +)))

(defn run []
  (let [expressions (->> (slurp "resources/input18.txt")
                   str/split-lines
                   (map #(read-string (str "(" % ")"))))]
    (println (solve expressions))))