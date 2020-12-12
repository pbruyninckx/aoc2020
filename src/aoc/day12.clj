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

(defn rotate-delta-left [[dx dy]]
  [(- dy) dx])
(defn rotate-delta-right [[dx dy]]
  [dy (- dx)])

(defn next-state2 [[[x y] [dx dy :as delta] :as state] [action n]]
  (case action
    \N (update-in state [1 1] #(+ % n))
    \S (update-in state [1 1] #(- % n))
    \W (update-in state [1 0] #(- % n))
    \E (update-in state [1 0] #(+ % n))
    \F (update state 0 #(mapv (fn [p dp] (+ p (* n dp))) % delta))
    \L (update state 1 #((apply comp (repeat (/ n 90) rotate-delta-left)) %))
    \R (update state 1 #((apply comp (repeat (/ n 90) rotate-delta-right)) %))))

(defn solve [directions]
  (let [start-state [0 0 \E]
        [x y _] (reduce next-state start-state directions)]
    (reduce + (map #(Math/abs ^int %) [x y]))))

(defn solve2 [directions]
  (let [start-state [[0 0] [10 1]]
        [pos & _] (reduce next-state2 start-state directions)]
    (reduce + (map #(Math/abs ^int %) pos))))

(defn read-data []
  (let [input-lines (->> (slurp "resources/input12.txt")
                        str/split-lines
                        (filter not-empty))]
    (mapv (fn [[action & number]]
            [action (Integer/parseInt (apply str number))])
          input-lines)))

(defn run []
  (let [directions (read-data)]
    (println (solve directions))
    (println (solve2 directions))))