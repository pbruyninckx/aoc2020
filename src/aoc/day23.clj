(ns aoc.day23
  (:require [clojure.string :as str]))

(defn parse-data [s]
  (->> s
       (map #(Integer/parseInt (str %)))
       vec))

(defn get-output [cups]
  (let [n (count cups)]
    (->> cups
         cycle
         (drop-while #(not= 1 %))
         rest
         (take (dec n))
         (map str)
         str/join)))

(defn get-destination [picked current]
  (letfn [(dec-loop [n] (if (= n 1) 9 (dec n)))]
    (let [picked (set picked)]
      (loop [result (dec-loop current)]
        (if (picked result)
          (recur (dec-loop result))
          result)))))

(defn solve [cups n]
  (if (= n 0)
    (get-output cups)
    (let [current (first cups)
          picked (take 3 (rest cups))
          dest (get-destination picked current)
          [before-dest from-dest] (split-with (complement #{dest}) (drop 4 cups))]
      (recur
        (concat before-dest [dest] picked (rest from-dest) [current])
        (dec n)))))

(def example "389125467")
(def input "219748365")

(defn run[]
  (let [data (parse-data input)]
    (println (solve data 100))))