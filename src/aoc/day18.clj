(ns aoc.day18
  (:require [clojure.string :as str]))

(defn evaluate [expression]
  (if (number? expression)
    expression
    (reduce (fn [acc [op expr2]]
               ((eval op) acc (evaluate expr2)))
             (evaluate (first expression))
             (partition 2 (rest expression)))))

(defn evaluate2 [expression]
  (if (or (number? expression) (symbol? expression))
    expression
    (apply *
           (reduce (fn [[acc+ acc*] el]
                     (cond (= el '*) [0 (* acc+ acc*)]
                           (= el '+) [acc+ acc*]
                           :else [(+ acc+ el) acc*]))
                   [0 1]
                   (map evaluate2 expression)))))

(defn solve [expressions ev-fn]
  (->> expressions
       (map ev-fn)
       (reduce +)))

(defn run []
  (let [expressions (->> (slurp "resources/input18.txt")
                   str/split-lines
                   (map #(read-string (str "(" % ")"))))]
    (doall (map #(println (solve expressions %))
                [evaluate evaluate2]))))
