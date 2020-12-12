(ns aoc.day12
  (:require [clojure.string :as str]))

(defn rotate-right [d]
  ({\N \E \E \S \S \W \W \N} d))

(defn rotate-left [d]
  ({\N \W \W \S \S \E \E \N} d))

(defn next-state [[x y d :as state] [action n]]
  (case action
    \N (update state 1 #(+ % n))
    \S (update state 1 #(- % n))
    \W (update state 0 #(- % n))
    \E (update state 0 #(+ % n))
    \F (recur state [d n])
    \L (update state 2 #((apply comp (repeat (/ n 90) rotate-left)) %))
    \R (update state 2 #((apply comp (repeat (/ n 90) rotate-right)) %))))

(defn solve [directions]
  (let [start-state [0 0 \E]
        [x y _] (reduce next-state start-state directions)]
    (reduce + (map #(Math/abs ^int %) [x y]))))

(defn read-data []
  (let [input-lines (->> (slurp "resources/input12.txt")
                        str/split-lines
                        (filter not-empty))]
    (mapv (fn [[action & number]]
            [action (Integer/parseInt (apply str number))])
          input-lines)))

(defn run []
  (let [directions (read-data)]
    (println (solve directions))))