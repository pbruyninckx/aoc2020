(ns aoc.day03
  (:require [clojure.java.io :as io]))

(defn solve [trees]
  (count
    (filter identity
            (map (fn [row pos] (get row (rem pos (count row))))
                 trees
                 (range 0 ##Inf 3)))))

(defn run []
  (with-open [rdr (io/reader "resources/input03.txt")]
    (let [trees (map #(into [] (map #{\#}) %) (line-seq rdr))]
      (println (solve trees)))))