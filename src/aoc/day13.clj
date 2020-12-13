(ns aoc.day13
  (:require [clojure.string :as str]))

(defn solve [start-time bus-numbers]
  (let [number
        (->> bus-numbers
             (apply min-key #(- % (rem start-time %))))]
    (* number (- number (rem start-time number)))))

(defn add-bus [[start-time step-size] [ind n]]
  (->> (range start-time (+ start-time (* step-size n)) step-size)
       (filter #(zero? (rem (+ % ind) n)))
       first
       (#(vector % (* step-size n)))))

(defn solve2 [indexed-bus-numbers]
  (first
    (reduce add-bus
            (first indexed-bus-numbers)
            (rest indexed-bus-numbers))))

(defn read-data[]
  (let [[start-time-str bus-nbr-str] (str/split-lines (slurp "resources/input13.txt"))
        bus-nbrs (->> (str/split bus-nbr-str #",")
                      (mapv #(if (= % "x")
                               -1
                               (Integer/parseInt %))))]
    [(Integer/parseInt start-time-str) bus-nbrs]))

(defn run[]
  (let [[start-time all-bus-numbers] (read-data)
        bus-numbers (filter pos-int? all-bus-numbers)
        indexed-bus (->> all-bus-numbers
                         (map-indexed vector)
                         (filter #(pos-int? (second %))))]
    (println (solve start-time bus-numbers))
    (println (solve2 indexed-bus))))