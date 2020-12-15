(ns aoc.day15)

(defn solve [numbers n]
  (let [start-locations (into {} (map-indexed #(vector %2 %1) (drop-last numbers)))
        last-number (last numbers)]
    (loop [seen (transient start-locations)
           last-number last-number
           i (count numbers)]
      (let [last-location (seen last-number)]
        (cond (= i n)
              last-number
              (not last-location)
              (recur (assoc! seen last-number (dec i)) 0 (inc i))
              :else
              (recur (assoc! seen last-number (dec i)) (- i last-location 1) (inc i)))))))

(defn run []
  (doall
    (map
      #(println (solve (long-array [20 9 11 0 1 2]) (long %)))
      [2020 30000000])))
