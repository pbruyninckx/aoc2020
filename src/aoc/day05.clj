(ns aoc.day05
  (:require [clojure.java.io :as io])
  (:require [clojure.set :as set]))

(defn seat-id [boarding-pass]
  (reduce
    (fn [acc val]
      ((if (#{\B \R} val) inc identity)
       (* 2 acc)))
    0
    boarding-pass))

(defn solve [boarding-passes]
  (reduce max (map seat-id boarding-passes)))

(defn solve2 [boarding-passes]
  (let [ids (set (map seat-id boarding-passes))]
    (first (filter
             #(and (not (ids %)) (ids (inc %)) (ids (dec %)))
             (range)))))

(defn run []
  (with-open [rdr (io/reader "resources/input05.txt")]
    (let [lines (line-seq rdr)]
      (println (solve lines))
      (println (solve2 lines)))))
