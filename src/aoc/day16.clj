(ns aoc.day16
  (:require [clojure.string :as str]))

(defn parse-constraint [line]
  (let [[min1 max1 min2 max2]
        (map #(Integer/parseInt %) (rest (str/split line #"\D+")))]
    (fn [val]
      (or (<= min1 val max1)
          (<= min2 val max2)))))

(defn parse-ticket [line]
  (mapv #(Integer/parseInt %) (str/split line #",")))

(defn parse-data [s]
  (let [[constraints-data my-data nearby-data]
        (->> s
             str/split-lines
             (partition-by empty?)
             (take-nth 2))]
    {:constraints (mapv parse-constraint constraints-data)
     :mine        (parse-ticket (second my-data))
     :nearby      (mapv parse-ticket (rest nearby-data))}))

(defn solve [data]
  (letfn [(valid-for-any-constraint [n]
            (some #(% n) (:constraints data)))]
    (->> (:nearby data)
         flatten
         (filter (complement valid-for-any-constraint))
         (reduce +))))

(defn run []
  (let [input-string (slurp "resources/input16.txt")
        data (parse-data input-string)]
    (println (solve data))))