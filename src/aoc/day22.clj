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

(defn recursive-combat [decks seen-decks]
  "Returns winner (0 or 1) and decks"
  (cond
    (seen-decks decks)
    [0 decks]
    (empty? (first decks))
    [1 decks]
    (empty? (second decks))
    [0 decks]
    :else
    (let [[c1 c2 :as top-cards] (map peek decks)
          [d1 d2 :as popped-decks] (map pop decks)
          winner
          (if (and (<= c1 (count d1))
                   (<= c2 (count d2)))
            (first (recursive-combat
                     (mapv (fn [c d] (queue (take c d)))
                           top-cards popped-decks)
                     #{}))
            (if (> c1 c2) 0 1))]
       (recur
         (update (vec popped-decks)
                 winner
                 #(reduce conj % (if (= winner 0) [c1 c2] [c2 c1])))
         (conj seen-decks decks)))))

(defn solve2 [data]
  (let [[winner decks] (recursive-combat data #{})]
    (get-score (nth decks winner))))

(defn run []
  (let [data (parse-data (slurp "resources/input22.txt"))]
    (println (solve data))
    (println (solve2 data))))
