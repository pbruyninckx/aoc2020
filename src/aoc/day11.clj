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

(defn sees-occupied-seat? [area [x y] delta]
  (letfn [(valid? [[x y]]
            (and (<= 0 x (dec (count (area 0))))
                 (<= 0 y (dec (count area)))))]
    (loop [[x y] (mapv + [x y] delta)]
      (cond (not (valid? [x y]))
            false
            (#{\#} (get-in area [y x]))
            true
            (#{\L} (get-in area [y x]))
            false
            :else
            (recur (mapv + [x y] delta))))))

(defn count-visible-seats [area [x y]]
  (count
    (filter
      identity
      (for [dx (range -1 2)
            dy (range -1 2)
            :when (not (= dx dy 0))]
        (sees-occupied-seat? area [x y] [dx dy])))))

(defn update-seat [area x y]
  (case (get-in area [y x])
    \. \.
    \# (if (>= (count-neighbours area [x y]) 4) \L \#)
    \L (if (= (count-neighbours area [x y]) 0) \# \L)))

(defn update-seat2 [area x y]
  (case (get-in area [y x])
    \. \.
    \# (if (>= (count-visible-seats area [x y]) 5) \L \#)
    \L (if (= (count-visible-seats area [x y]) 0) \# \L)))

(defn update-area [area update-fn]
  (let [[w h] (size area)]
    (mapv (fn [y]
            (mapv #(update-fn area % y) (range w)))
          (range h))))

(defn print-area [area]
  (doall (map #(println (apply str %)) area))
  (newline))

(defn solve [area update-fn]
  (loop [area area]
    (let [new-area (update-area area update-fn)]
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
    (println (solve area update-seat))
    (println (solve area update-seat2))))