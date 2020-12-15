(ns aoc.day14
  (:require [clojure.string :as str])
  (:require [clojure.math.combinatorics :as combo]))

(defn parse-line [s]
  (if (str/starts-with? s "mask")
    (let [mask (subs s 7)]
      {:type :mask
       :value
             {:raw  mask
              :and  (Long/parseLong (str/replace mask #"X" "1") 2)
              :or   (Long/parseLong (str/replace mask #"X" "0") 2)
              :Xpos (->> mask
                         reverse
                         (map-indexed vector)
                         (filter #(= \X (% 1)))
                         (mapv #(% 0)))}})
    {:type :mem
     :address (Long/parseLong (re-find #"\d+" s))
     :value (Long/parseLong ((str/split s #" = ") 1))}))

(defn apply-mask [value mask]
  (-> value
      (bit-and (:and mask))
      (bit-or (:or mask))))

(defn update-memory [memory instruction mask]
  (assoc memory
    (:address instruction)
    (apply-mask (:value instruction) mask)))

(defn get-masked-addresses [address mask]
  "The different memory addresses after applying the mask"
  (let [base-memory (bit-or address (:or mask))
        bit-positions (mask :Xpos)
        bit-functions (mapv
                        (fn [n]
                          [#(bit-clear % n) #(bit-set % n)])
                        bit-positions)
        all-fun-combos (mapv #(apply comp %) (apply combo/cartesian-product bit-functions))]
    (map #(% base-memory) all-fun-combos)))

(defn update-memory2 [memory instruction mask]
  (reduce (fn [memory address]
            (assoc memory address (:value instruction)))
          memory
          (get-masked-addresses (:address instruction) mask)))

(defn solve [instructions update-memory-fn]
  (loop [memory {}
         mask nil
         [instruction & remaining] instructions]
    (cond (not instruction)
          (reduce + (vals memory))
          (= (:type instruction) :mask)
          (recur memory (:value instruction) remaining)
          :else
          (recur (update-memory-fn
                   memory
                   instruction
                   mask)
                 mask
                 remaining))))

(defn read-data []
  (let [lines (str/split-lines (slurp "resources/input14.txt"))]
    (mapv parse-line lines)))

(defn run []
  (let [data (read-data)]
    (println (solve data update-memory))
    (println (solve data update-memory2))))
