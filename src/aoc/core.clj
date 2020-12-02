(ns aoc.core
  (:gen-class))

(require '[aoc.day01 :as day01])

(defn -main
  "Runs the code for the given day"
  [day & args]
  (let [d (Integer/parseInt day)
        day-space (format "aoc.day%02d" d)]
    (require (symbol day-space))
    ((resolve (symbol day-space "run")))))
