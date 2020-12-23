(ns aoc.day19
  (:require [clojure.string :as str]))

(defn parse-rule [s]
  (letfn [(parse-int [s]
            (cond (= s "|") nil
                  (= \" (first s)) (second s)
                  :else
                  (Integer/parseInt s)))]
    (let [[n & es] (->> (str/split s #":? ")
                        (map parse-int))]
      [n (if (char? (first es))
           (first es)
           (->> es
                (partition-by boolean)
                (take-nth 2)
                (map vec)
                vec))])))

(defn parse-rules [rule-data]
  (let [empty-ret (vec (repeat (count rule-data) nil))]
    (reduce (fn [acc [n rule]] (assoc acc n rule))
            empty-ret
            (map parse-rule rule-data))))

(defn parse-data [data]
  (let [[rules-data messages]
        (->> data
             str/split-lines
             (partition-by empty?)
             (take-nth 2))]
    {:rules (parse-rules rules-data)
     :messages messages}))

(defn consume-letter [rules [current & remaining :as state] letter]
  (cond
    (nil? current) nil
    (char? current) (if (= letter current) [(rest state)] nil)
    (coll? current)
    (apply
      concat
      (map #(consume-letter rules (concat % remaining) letter)
           current))
    :else ; Let's recurse
    (recur rules (cons (rules current) remaining) letter)))

(defn matches? [rules message]
  (loop [states (rules 0)
         [c & remaining] message]
    (if (nil? c)
      (some empty? states)
      (recur
        (->> states
             (map #(consume-letter rules % c))
             (apply concat))
        remaining))))

(defn solve [{:keys [rules messages]}]
  (->> messages
       (map #(matches? rules %))
       (filter identity)
       count))

(defn change-rules [data]
  (-> data
      (assoc-in [:rules 8] [[42] [42 8]])
      (assoc-in [:rules 11] [[42 31] [42 11 31]])))

(defn run[]
  (let [data (parse-data (slurp "resources/input19.txt"))]
    (println (solve data))
    (println (solve (change-rules data)))))
