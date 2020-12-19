(ns aoc.day17
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn flatten-1 [l]
  (mapcat seq l))

(defn parse-data [s]
  (let [indexed-data
        (map-indexed
          (fn [row-index row]
            (map-indexed (fn [col-index char]
                           ({\# [row-index col-index 0 0]} char))
                         row))
          (str/split-lines s))]
    (->> indexed-data
         flatten-1
         (filter identity)
         set)))

(defn neighbours [[x y z t]]                                  ; xy- switch doesn't matter here
  (letfn [(neighbours-1 [el] (range (dec el) (+ 2 el)))]
    (for [xn (neighbours-1 x)
          yn (neighbours-1 y)
          zn (neighbours-1 z)
          tn (neighbours-1 t)
          :when (not (and (= x xn) (= y yn) (= z zn) (= t tn)))]
      [xn yn zn tn])))

(defn next-cycle [data]
  (let [current-neighbours
        (set/difference (->> data
                             (map neighbours)
                             flatten-1
                             set)
                        data)]
    (set/union
      (->> data
           (filter #(<= 2
                        (count (set/intersection
                                 (set (neighbours %))
                                 data))
                        3))
           set)
      (->> current-neighbours
           (filter #(= 3
                       (count (set/intersection
                                (set (neighbours %))
                                data))))
           set))))

(defn solve [data]
  (loop [data data n 0]
    (if (= n 6)
      (count data)
      (recur (next-cycle data) (inc n)))))

(defn run[]
  (let [data (parse-data (slurp "resources/input17.txt"))]
    (println (solve data))))
