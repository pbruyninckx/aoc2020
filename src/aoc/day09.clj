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

(defn solve2 [numbers target]
  (let [cumsum (vec (reductions + numbers))]
    (filter identity
            (for [max-ind (range 1 (count numbers))
                  min-ind (range 0 (count numbers))]
              (if (and (= target (- (cumsum max-ind) (nth cumsum (dec min-ind) 0)))
                       (not= min-ind max-ind))
                (let [solution-range (->> numbers (drop min-ind) (take (inc (- max-ind min-ind))))]
                  (+ (apply min solution-range) (apply max solution-range)))
                nil)))))

(defn run[]
  (with-open [rdr (io/reader "resources/input09.txt")]
    (let [lines (line-seq rdr)
          numbers (map #(Long/parseLong %) lines)
          solution1 (solve numbers)]
      (println solution1)
      (println (solve2 numbers solution1)))))
