(ns aoc.day01
  (:require [clojure.java.io :as io]))

(defn find-pair [seen to-see]
  (let [[el & remaining] to-see
        complement (- 2020 el)]
    (if (seen complement)
      [el complement]
      (recur (conj  seen el) remaining))))

(defn solve [numbers]
  (let [[a b] (find-pair #{} numbers)]
    (* a b)))

(defn day1 []
  (with-open [rdr (io/reader "resources/input01.txt")]
    (println (solve (map #(Integer/parseInt %) (line-seq rdr))))))