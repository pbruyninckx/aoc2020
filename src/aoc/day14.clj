(ns aoc.day14
  (:require [clojure.string :as str]))

(defn parse-line [s]
  (if (str/starts-with? s "mask")
    (let [mask (subs s 7)]
      {:type :mask
       :value
             {:and (Long/parseLong (str/replace mask #"X" "1") 2)
              :or  (Long/parseLong (str/replace mask #"X" "0") 2)}})
    {:type :mem
     :address (Long/parseLong (re-find #"\d+" s))
     :value (Long/parseLong ((str/split s #" = ") 1))}))


(defn apply-mask [value mask]
  (-> value
      (bit-and (:and mask))
      (bit-or (:or mask))))

(defn solve [instructions]
  (loop [memory {}
         mask nil
         [instruction & remaining] instructions]
    (cond (not instruction)
          (reduce + (vals memory))
          (= (:type instruction) :mask)
          (recur memory (:value instruction) remaining)
          :else
          (recur (assoc
                   memory
                   (:address instruction)
                   (apply-mask (:value instruction) mask))
                 mask
                 remaining))))

(defn read-data []
  (let [lines (str/split-lines (slurp "resources/input14.txt"))]
    (mapv parse-line lines)))

(defn run []
  (let [data (read-data)]
    (println (solve data))))
