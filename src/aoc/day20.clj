(ns aoc.day20
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-tile [[header & tile]]
  [;Get tile number
   (->> header
        (re-find #"\d+")
        (Integer/parseInt))
   ;Get actual tile as vector of vectors for fast access
   (mapv vec tile)])

(defn parse-data [s]
  (->> s
       str/split-lines
       (partition-by empty?)
       (take-nth 2)
       (map parse-tile)
       (into {})))

(defn get-borders [tile direction]
  (let [borders
        (mapv #(% tile)
        [first last #(mapv first %) #(mapv last %)])
        reverse-borders
        (map #(vec (reverse %)) borders)]
    (case direction
      :both (into borders reverse-borders)
      :orig borders)))

(defn get-corner-tiles [data]
  (let [edge-borders
        (->> data
             vals
             (map #(get-borders % :both))
             (mapcat seq)
             frequencies
             (filter #(= 1  (% 1)))
             (map #(% 0))
             set)]
    (letfn [(is-corner-tile [[_ tile]]
              (->> (get-borders tile :orig)
                   set
                   (set/intersection edge-borders)
                   count
                   (= 2)))]
      (->> data
           (filter is-corner-tile)))))


(defn solve [data]
  (->> data
       get-corner-tiles
       (map #(% 0))
       (apply *)))

(defn run []
  (let [data (parse-data (slurp "resources/input20.txt"))]
    (println (solve data))))