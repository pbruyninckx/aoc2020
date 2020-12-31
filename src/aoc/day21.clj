(ns aoc.day21
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [s]
  (let [[all-ingredients all-allergens]
        (str/split s #" \(contains |\)")]
    {:ingredients (set (str/split all-ingredients #" "))
     :allergens   (set (str/split all-allergens #", "))}))

(defn parse-data [input-text]
  (->> input-text
       str/split-lines
       (map parse-line)))

(defn get-all-of [kind data]
  (->> data
       (map kind)
       (reduce set/union)))

(defn get-all-ingredients [data]
  (get-all-of :ingredients data))

(defn get-all-allergens [data]
  (get-all-of :allergens data))

(defn get-possible-allergen-assignments [data]
  "Returns a map {allergen : #{ingredients} }"
  (reduce
    (fn [result {:keys [:ingredients :allergens]}]
      (reduce (fn [sub-result allergen]
                (if (sub-result allergen)
                  (update sub-result allergen #(set/intersection ingredients %))
                  (assoc sub-result allergen ingredients)))
              result
              allergens))
    {}
    data))

(defn solve [data]
  (let [possible-assignments
        (get-possible-allergen-assignments data)
        allergen-free-ingredients
        (reduce set/difference (get-all-ingredients data) (vals possible-assignments))]
    (->> data
         (map :ingredients)
         (map #(set/intersection % allergen-free-ingredients))
         (map count)
         (reduce +))))

(defn get-assignments [possible-assignments]
  (loop [assignments {}
         to-assign possible-assignments]
    (if
      (empty? to-assign)
      assignments
      (let [[ingredient [allergen & _]]
            (->> to-assign
                 (sort-by (fn [[_ ingredients]] (count ingredients)))
                 first)]
        (recur
          (assoc assignments ingredient allergen)
          (->> to-assign
               (filter #(not= ingredient (first %)))
               (map #(update % 1 (fn [st] (disj st allergen))))))))))

(defn solve2 [data]
  (let [possible-assignments
        (get-possible-allergen-assignments data)]
    (->> possible-assignments
         get-assignments
         (sort-by first)
         (map second)
         (str/join ","))))

(defn run []
  (let [data (parse-data (slurp "resources/input21.txt"))]
    (println (solve data))
    (println (solve2 data))))
