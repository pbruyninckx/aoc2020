(ns aoc.day23
  (:require [clojure.string :as str]))

(defn parse-data [s]
  "Returns s as ints: first element, and next-value 'map'"
  (let [n (count s)
        ret (vec (repeat (inc n) 0))
        next-val-map
        (->> s
             (map #(Integer/parseInt (str %)))
             cycle
             (partition 2 1)
             (map vec)
             (take n)
             (into {0 0}))]
    [(Integer/parseInt (str (first s)))
     (mapv next-val-map (range (inc n)))]))

(defn to-seq [start-val next-val]
  (lazy-seq (cons start-val (to-seq (next-val start-val) next-val))))

(defn move [current next-val]
  (let [[a b c] (take 3 (rest (to-seq current next-val)))
        next-dest (loop [val (dec current)]
                    (cond (= val 0) (recur (dec (count next-val)))
                          (#{a b c} val) (recur (dec val))
                          :else val))]
    [(next-val c)
     (-> next-val
         (assoc current (next-val c))
         (assoc c (next-val next-dest))
         (assoc next-dest a))]))

(defn print-seq [current next-val]
  (println (take (dec (count next-val)) (to-seq current next-val))))

(defn move-cups [current next-val num-iters]
  (if
    (= num-iters 0)
    [current next-val]
    (let [[current next-val] (move current next-val)]
      (recur current next-val (dec num-iters)))))

(defn solve [data]
  (let [[start-value initial-next-values] data
        [_ final-next-values] (move-cups start-value initial-next-values 100)
        ]
    (->> (to-seq 1 final-next-values)
         rest
         (take (- (count initial-next-values) 2))
         (map str)
         str/join)))

(defn solve2 [data]
  (let [[start incomplete-next] data
        n (count incomplete-next)
        initial-next (conj
                       (into (mapv #(if (= start %) n %) incomplete-next)
                             (range (inc n) (inc 1000000)))
                       start)]
    (->> (move-cups start initial-next 10000000)
         second
         (to-seq 1)
         rest
         (take 2)
         (reduce *))))

(def example "389125467")
(def input "219748365")

(defn run[]
  (let [data (parse-data input)]
    (println (solve data))
    (println (solve2 data))))