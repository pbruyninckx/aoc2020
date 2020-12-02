(ns aoc.day02
  (:require [clojure.java.io :as io]))

(def line-re
  #"(?<min>\d+)-(?<max>\d+) (?<char>.): (?<code>\w+)" )

(defn parse-line [line]
  (let [matcher (re-matcher line-re line)]
    (.matches matcher)
    {:min  (Integer/parseInt (.group matcher "min"))
     :max  (Integer/parseInt (.group matcher "max"))
     :char (first (.group matcher "char"))
     :code (.group matcher "code")
     }))

(defn valid-password? [{:keys [min max char code]}]
  (<= min
      (count (filter #{char} code))
      max))

(defn valid-password2? [{:keys [min max char code]}]
  (not= (= char (get code (dec min)))
        (= char (get code (dec max)))))

(defn solve [passwords]
  (count (filter valid-password? passwords)))

(defn solve2 [passwords]
  (count (filter valid-password2? passwords)))

(defn run []
  (with-open [rdr (io/reader "resources/input02.txt")]
    (let [passwords (map parse-line (line-seq rdr))]
      (println (solve passwords))
      (println (solve2 passwords)))))
