(ns aoc.day24
  (:require [clojure.string :as str]))

(defn parse-line [s]
  (cond
    (empty? s) '()
    (#{\e \w} (first s)) (lazy-seq (cons (keyword (str (first s)))
                                        (parse-line (rest s))))
    :else (lazy-seq (cons (keyword (apply str (take 2 s)))
                          (parse-line (drop 2 s))))))

(defn parse-input [input-text]
  (->> input-text
       (str/split-lines)
       (map parse-line)))

(def direction-to-coords
  {:e  [1 0]
   :se [1 -1]
   :sw [0 -1]
   :w  [-1 0]
   :nw [-1 1]
   :ne [0 1]})

(defn final-location [directions]
  (->> directions
       (map direction-to-coords)
       (reduce #(mapv + %1 %2))))

(defn solve [data]
  (->> data
       (map final-location)
       (reduce (fn [black loc]
                 (if (black loc)
                   (disj black loc)
                   (conj black loc)))
               #{})
       count))

(defn run[]
  (let [data (parse-input (slurp "resources/input24.txt"))]
    (println (solve data))))
