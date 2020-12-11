(ns aoc.day11
  (:require [clojure.string :as str]))

(defn size [area]
  [(count (area 0)) (count area)])

(defn count-neighbours [area [x y]]
  (let [[w h] (size area)]
    (count
      (filter
        identity
        (for [indx (range (dec x) (+ 2 x))
              indy (range (dec y) (+ 2 y))
              :when (not (and (= x indx) (= y indy)))]
          (= (get-in area [indy indx] \.) \#))))))

(defn update-seat [area x y]
  (case (get-in area [y x])
    \. \.
    \# (if (>= (count-neighbours area [x y]) 4) \L \#)
    \L (if (= (count-neighbours area [x y]) 0) \# \L)))

(defn update-area [area]
  (let [[w h] (size area)]
    (mapv (fn [y]
            (mapv #(update-seat area % y) (range w)))
          (range h))))

(defn print-area [area]
  (doall (map #(println (apply str %)) area))
  (newline))

(defn solve [area]
  (loop [area area]
    (let [new-area (update-area area)]
      (if (= area new-area)
        (->> area flatten (filter #{\#}) count)
        (recur new-area)))))

(defn read-data []
  (let [input-text (slurp "resources/input11.txt")
        input-lines (str/split-lines input-text)]
    (->> input-lines
         (mapv #(mapv identity %)))))

(defn run[]
  (let [area (read-data)]
    (println (solve area))))