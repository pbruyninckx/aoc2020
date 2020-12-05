(ns aoc.day05
  (:require [clojure.java.io :as io]))

(defn seat-id [boarding-pass]
  (reduce
    (fn [acc val]
      ((if (#{\B \R} val) inc identity)
       (* 2 acc)))
    0
    boarding-pass))

(defn solve [boarding-passes]
  (reduce max (map seat-id boarding-passes)))

(defn run []
  (with-open [rdr (io/reader "resources/input05.txt")]
    (let [lines (line-seq rdr)]
      (println (solve lines)))))
