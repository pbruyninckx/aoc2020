(ns aoc.day25
  (:require [clojure.string :as str]))

(def p
  "Modulo number during transformations"
  20201227)

(def subject-number
  7)

(defn transform
  ([subject-number n]
    (rem (* n subject-number) p))
  ([subject-number n times]
   (->> n
        (iterate (partial transform subject-number))
        (drop (dec times))
        first)))

(defn transformer [subject-number]
  (partial transform subject-number))

(defn parse-input [input]
  (->> input
       str/split-lines
       (mapv #(Long/parseLong %))))

(defn solve [[card-key door-key :as both-keys]]
  (let [looped-numbers (->> 7
                            (iterate (transformer 7))
                            (map-indexed #(vector (inc %1) %2)))
        keys-set (set both-keys)]
    (->> looped-numbers
         (filter (fn [[_ val]] (keys-set val)))
         first
         first ; loop size of card
         (transform door-key door-key))))

(defn run []
  (let [[card-key door-key :as both-keys] (parse-input (slurp "resources/input25.txt"))]
    (println (solve both-keys))))
