(ns aoc.day06
  (:require [clojure.java.io :as io])
  (:require [clojure.set :as set]))

(defn solve [lines]
  (->> lines
       (partition-by empty?)
       (take-nth 2)
       (map #(map set %))
       (map #(apply set/union %))
       (map count)
       (reduce +)))

(defn run []
  (with-open [rdr (io/reader "resources/input06.txt")]
    (let [lines (line-seq rdr)]
      (println (solve lines)))))
