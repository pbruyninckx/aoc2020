(ns aoc.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defrecord instruction [op arg])

(defn parse-program [lines]
  (letfn [(parse-line [line]
            (let [[op arg] (str/split line #" ")]
              (->instruction op (Integer/parseInt arg))))]
    (mapv parse-line lines)))

(defrecord console [instructions line acc])

(defn step [^console {:keys [instructions line] :as console}]
  (let [{:keys [op arg]} (instructions line)]
    (case op
      "nop"
      (update console :line inc)
      "acc"
      (-> console
          (update :line inc)
          (update :acc #(+ % arg)))
      "jmp"
      (update console :line #(+ % arg)))))

(defn new-console [program]
  (->console program 0 0))

(defn solve [program]
  (loop [console (new-console program)
         executed #{}]
    (if (executed (:line console))
      (:acc console)
      (recur (step console)
             (conj executed (:line console))))))

(defn run []
  (with-open [rdr (io/reader "resources/input08.txt")]
    (let [lines (line-seq rdr)
          program (parse-program lines)]
      (println (solve program)))))
