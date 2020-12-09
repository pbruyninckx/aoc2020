(ns aoc.day09
  (:require [clojure.java.io :as io]))

(defn find-pair [seen to-see total]
  (if (not to-see)
    false
    (let [[el & remaining] to-see
          complement (- total el)]
      (if (seen complement)
        true
        (recur (conj seen el) remaining total)))))

(defn sums-to-last? [numbers]
  (find-pair #{} (drop-last numbers) (last numbers)))

(defn solve [numbers]
  (->> numbers
       (partition 26 1)
       (filter (complement sums-to-last?))
       first
       last))

(defn run[]
  (with-open [rdr (io/reader "resources/input09.txt")]
    (let [lines (line-seq rdr)
          numbers (map #(Long/parseLong %) lines)]
      (println (solve numbers)))))
