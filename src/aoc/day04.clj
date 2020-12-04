(ns aoc.day04
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn parse-password [passport-data]
  (into {}
        (map #(str/split % #":")
             passport-data)))

(defn read-passports [lines]
  (letfn [(read-flattened-passports [lines]
            (let [[passport-data rem-lines-empty-start]
                  (split-with not-empty lines)
                  rem-lines (drop-while empty? rem-lines-empty-start)]
              (if (and passport-data (not-empty passport-data))
                (lazy-seq (cons (parse-password passport-data)
                                (read-flattened-passports rem-lines)))
                '())))]
    (let [expanded-lines
          (->> lines
               (map #(str/split % #" "))
               flatten)]
      (read-flattened-passports expanded-lines))))

(defn valid-passport? [passport]
  (empty? (set/difference
            #{"byr"
              "iyr"
              "eyr"
              "hgt"
              "hcl"
              "ecl"
              "pid"}
            (set (keys passport)))))

(defn solve [passports]
  (count (filter valid-passport? passports)))

(defn run []
  (with-open [rdr (io/reader "resources/input04.txt")]
    (let [lines (line-seq rdr)
          passports (read-passports lines)]
      (println (first passports))
      (println (solve passports)))))
