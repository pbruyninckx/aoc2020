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

(defn subsets [[el & remaining :as s] n]
  (cond
    (= n 0) '(())
    (< (count s) n) '()
    :else
    (lazy-cat
      (map #(conj % el) (subsets remaining (dec n)))
      (subsets remaining n))))

(defn solve2 [numbers]
  (let [triplets (subsets numbers 3)
        [solution-triplet] (filter #(= 2020 (reduce + %)) triplets)]
    (reduce * solution-triplet)))

(defn run []
  (with-open [rdr (io/reader "resources/input01.txt")]
    (let [numbers (map #(Integer/parseInt %) (line-seq rdr))]
      (println (solve numbers))
      (println (solve2 numbers)))))