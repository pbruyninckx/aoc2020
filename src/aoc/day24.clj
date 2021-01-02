(ns aoc.day24
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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

(def neighbours
  (vals direction-to-coords))

(defn get-neighbours [coord]
  (map #(mapv + coord %) neighbours))

(defn flatten-1 [l]
  (mapcat seq l))

(defn inc-ext [x]
  "extended inc returning 1 if input is nil"
  (if x
    (inc x)
    1))

(defn update-tile [old-black]
  (let [neighbour-count
        (->> old-black
             (map get-neighbours)
             flatten-1
             (reduce
               (fn [ret coord] (update ret coord inc-ext))
               {}))]
    (-> old-black
        (set/intersection (set (keys neighbour-count)))
        (set/difference (set (map first (filter #(> (second %) 2) neighbour-count))))
        (set/union (set/difference (set (map first (filter #(= (second %) 2) neighbour-count)))
                                   old-black)))))

(defn final-location [directions]
  (->> directions
       (map direction-to-coords)
       (reduce #(mapv + %1 %2))))

(defn initial-tiles [data]
  (->> data
       (map final-location)
       (reduce (fn [black loc]
                 (if (black loc)
                   (disj black loc)
                   (conj black loc)))
               #{})))

(defn solve [data]
  (->> data
       initial-tiles
       count))

(defn solve2 [data]
  (->> data
       initial-tiles
       (iterate update-tile)
       (drop 100)
       first
       count))

(defn run[]
  (let [data (parse-input (slurp "resources/input24.txt"))]
    (println (solve data))
    (println (solve2 data))))
