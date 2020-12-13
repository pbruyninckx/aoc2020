(ns aoc.day13
  (:require [clojure.string :as str]))

(defn solve [start-time bus-numbers]
  (let [number
        (->> bus-numbers
             (apply min-key #(- % (rem start-time %))))]
    (* number (- number (rem start-time number)))))

(defn read-data[]
  (let [[start-time-str bus-nbr-str] (str/split-lines (slurp "resources/input13.txt"))
        bus-nbrs (->> (str/split bus-nbr-str #",")
                      (mapv #(if (= % "x")
                               -1
                               (Integer/parseInt %))))]
    [(Integer/parseInt start-time-str) bus-nbrs]))

(defn run[]
  (let [[start-time all-bus-numbers] (read-data)
        bus-numbers (filter pos-int? all-bus-numbers)]
    (println (solve start-time bus-numbers))))