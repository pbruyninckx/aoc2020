(ns aoc.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defrecord num-colour [^int amount ^String colour])
(defrecord rule [^String left right])

(defn parse-num-colour [s]
  (let [[n colour] (str/split s #" " 2)]
    (->num-colour (Integer/parseInt n) colour)))

(defn parse-rule [line]
  (let [[left right] (str/split line #" bags contain " 2)
        right-split
        (if (= right "no other bags.")
          []
          (str/split right #" bags?((, )|\.)"))]
    (->rule left
            (mapv parse-num-colour right-split))))

(defn invert-rule[^rule rule]
  (into {} (map
             #(vector (:colour %) #{(:left rule)})
             (:right rule))))

(defn get-parents-of [start parents]
  (loop [seen #{}
         to-see #{start}
         result #{}]
    (cond (empty? to-see) result
          (seen (first to-see)) (recur seen (rest to-see) result)
          :else
          (recur
            (conj seen (first to-see))
            (set/union to-see (set/difference
                                (parents (first to-see))
                                seen))
            (conj result (first to-see))))))

(defn solve [rules]
  (let [inverted-rules (map invert-rule rules)
        parents (reduce #(merge-with set/union %1 %2) inverted-rules)]
    (dec (count (get-parents-of "shiny gold" parents)))))

(defn run []
  (with-open [rdr (io/reader "resources/input07.txt")]
    (let [lines (line-seq rdr)
          rules (map parse-rule lines)]
      (println (solve rules)))))
