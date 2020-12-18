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

(defn remove-invalid-tickets [{:keys [nearby constraints] :as data}]
  (letfn [(invalid-for-all-constraints [n]
            (not-any? #(% n) constraints))]
    (->> nearby
         (filter (complement #(some invalid-for-all-constraints %)))
         (vec)
         (assoc data :nearby))))

(defn get-col [n nearby]
  (mapv #(get % n) nearby))

(defn get-constraint-matrix [{:keys [constraints nearby]}]
  "each row is a constraint, each column applies to certain field values"
  (mapv vec
        (partition (count constraints)
                   (for [constraint constraints
                         column (map #(get-col % nearby) (range (count (nearby 0))))]
                     (every? constraint column)))))

(defn print-mat [mat]
  (doall
    (map #(println (apply str %))
         (partition 20 (map {false \. true \#}
                            mat)))))

(defn resolve-constraints [mat correspondence n]
  (cond
    (= n (count correspondence))
    correspondence
    :else
    (->> (range (count correspondence))
         (filter #(not-any? #{%} correspondence))
         (filter #(get-in mat [n %]))
         (sort-by #(count(filter identity (get-col % (drop n mat)))))
         (map #(resolve-constraints mat (assoc correspondence n %) (inc n)))
         first)))

(defn solve2 [data]
  (let [constraint-matrix (get-constraint-matrix data)]
    (let [mappings (resolve-constraints constraint-matrix
                         (vec (repeat (count constraint-matrix) nil))
                         0)]
      (reduce * (map #((:mine data) %) (take 6 mappings))))))

(defn run []
  (let [input-string (slurp "resources/input16.txt")
        data (parse-data input-string)
        valid-data (remove-invalid-tickets data)]
    (println (solve data))
    (println (solve2 valid-data))))