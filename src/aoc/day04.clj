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

(defn is-number-and-between? [s d low high]
  (and (re-matches (case d 2 #"\d{2}" 3 #"\d{3}" 4 #"\d{4}") s)
       (let [n (Integer/parseInt s)]
         (<= low n high))))

(defn valid-passport-fields? [passport]
  (and (valid-passport? passport)
       (is-number-and-between? (passport "byr") 4 1920 2002)
       (is-number-and-between? (passport "iyr") 4 2010 2020)
       (is-number-and-between? (passport "eyr") 4 2020 2030)
       (let [full-height (passport "hgt")
             number-str (str/join (drop-last 2 full-height))
             unit (take-last 2 full-height)]
         (cond (= unit [\c \m])
               (is-number-and-between? number-str 3 150 193)
               (= unit [\i \n])
               (is-number-and-between? number-str 2 59 76)
               :else
               false))
       (re-matches #"#[0-9a-f]{6}" (passport "hcl"))
       (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} (passport "ecl"))
       (re-matches #"\d{9}" (passport "pid"))))

(defn solve [passports]
  (count (filter valid-passport? passports)))

(defn solve2 [passports]
  (count (filter valid-passport-fields? passports)))

(defn run []
  (with-open [rdr (io/reader "resources/input04.txt")]
    (let [lines (line-seq rdr)
          passports (read-passports lines)]
      (println (solve passports))
      (println (solve2 passports)))))
