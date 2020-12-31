(ns aoc.day22
  (:require [clojure.string :as str])
  (:import (clojure.lang PersistentQueue)))

(defn queue
  ([] (PersistentQueue/EMPTY))
  ([coll]
   (reduce conj PersistentQueue/EMPTY coll)))

(defn parse-data [input]
  (->> input
       str/split-lines
       (partition-by empty?)
       (take-nth 2)
       (map rest)
       (map #(map (fn [s] (Integer/parseInt s)) %))
       (map queue)))

(defn get-score [deck]
  (->> deck
       reverse
       (map (fn [ind val] (* ind val)) (range 1 ##Inf))
       (reduce +)))

(defn solve [decks]
  (if (some empty? decks)
    (->> decks
         (filter not-empty)
         first
         get-score)
    (let [[c1 c2] (map peek decks)
          [d1 d2] (map pop decks)]
      (recur (if (> c1 c2)
               [(reduce conj d1 [c1 c2]) d2]
               [d1 (reduce conj d2 [c2 c1])])))))

(defn run []
  (let [data (parse-data (slurp "resources/input22.txt"))]
    (println (solve data))))
