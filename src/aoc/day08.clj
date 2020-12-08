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

(defn terminated? [^console {:keys [instructions line]}]
  (= line (count instructions)))

(defn new-console [program]
  (->console program 0 0))

(defn run-program [program]
  (loop [console (new-console program)
         executed #{}]
    (if (or (executed (:line console))
            (terminated? console))
      console
      (recur (step console)
             (conj executed (:line console))))))

(defn solve [program]
  (:acc (run-program program)))

(defn changed-programs [program]
  "The problem is small enough to brute force"
  (letfn [(can-switch [^instruction instruction]
            (#{"jmp" "nop"} (:op instruction)))
          (switch-arg [^instruction instruction]
            (case (:op instruction)
              "jmp" (assoc instruction :op "nop")
              "nop" (assoc instruction :op "jmp")))]
    (map #(update program % switch-arg)
         (filter #(can-switch (program %))
                 (range (count program))))))


(defn solve2 [program]
  (->> (changed-programs program)
       (map run-program)
       (filter terminated?)
       first
       :acc))

(defn run []
  (with-open [rdr (io/reader "resources/input08.txt")]
    (let [lines (line-seq rdr)
          program (parse-program lines)]
      (println (solve program))
      (println (solve2 program)))))
