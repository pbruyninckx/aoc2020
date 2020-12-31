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

(def border-order
  [:top :bottom :left :right :rev-top :rev-bottom :rev-left :rev-right])

(defn get-borders [tile direction]
  (let [borders
        (mapv #(% tile)
              [first last #(mapv first %) #(mapv last %)])
        reverse-borders
        (map #(vec (reverse %)) borders)]
    (case direction
      :both (into borders reverse-borders)
      :orig borders)))

(defn rotate
  ([degrees tile]
   "Performs an anti-clockwise rotate"
   (rotate degrees :no-flip tile))
  ([degrees do-flip tile]
   "Performs an (optional) horizontal flip, followed by an anti-clockwise rotate"
   (let [size-change (case degrees (0 180) false (90 270) true)
         [n-rows n-cols] ((if size-change reverse identity)
                          [(count tile) (count (first tile))])]
     (letfn [(transform [fun]
               (->> (for [row (range n-rows) col (range n-cols)]
                              (get-in tile (fun row col)))
                    (partition n-cols)
                    (mapv vec)))
             (neg-r [val] (- n-rows val 1))
             (neg-c [val] (- n-cols val 1))]
(case [degrees do-flip]
         [0 :no-flip] (mapv vec tile)
         [90 :no-flip] (transform (fn [r c] [c (neg-r r)]))
         [180 :no-flip] (transform (fn [r c] [(neg-r r) (neg-c c)]))
         [270 :no-flip] (transform (fn [r c] [(neg-c c) r]))
         [0 :flip] (transform (fn [r c] [r (neg-c c)]))
         [90 :flip] (transform (fn [r c] [c r]))
         [180 :flip] (transform (fn [r c] [(neg-r r) c]))
         [270 :flip] (transform (fn [r c] [(neg-c c) (neg-r r)])))))))



(defn get-edge-frequencies [data]
  (->> data
       vals
       (map #(get-borders % :both))
       (mapcat seq)
       frequencies))

(defn get-edge-borders [data]
  (->> data
       get-edge-frequencies
       (filter #(= 1 (% 1)))
       (map #(% 0))
       set))

(defn is-corner-tile [edge-borders [_ tile]]
  (->> (get-borders tile :orig)
       set
       (set/intersection edge-borders)
       count
       (= 2)))

(defn get-corner-tiles
  ([data]
   (get-corner-tiles data (get-edge-borders data)))
  ([data edge-borders]
   (->> data
        (filter #(is-corner-tile edge-borders %)))))

(defn get-top-left-tile [data]
  "Returns a corner tile as [id transformed-tile],
  with the tile-data transformed to bottom and left are borders"
  (let [edge-borders (get-edge-borders data)
        corner-tile (first (get-corner-tiles data edge-borders))
        corner-tile-borders (get-borders (second corner-tile) :orig)]
    (let [border-sides
          (->> corner-tile-borders
               (map-indexed vector)
               (filter #(edge-borders (% 1)))
               (map #(border-order (% 0))))]
      (cond
        (= border-sides '(:bottom :right))
        (update corner-tile 1 #(rotate 180 %))
        (= border-sides '(:bottom :left))
        (update corner-tile 1 #(rotate 270 %))
        :else
        (throw (Exception. "I only implemented the cases I needed (example + real data)"))))))

(defn edge-from-wave-coords [wave-coords [row col :as offset] orientation n]
  (->> (range n)
       (map (if (= orientation :col)
              #(vector (+ row %) col)
              #(vector row (+ col %))))
       (mapv #(if (wave-coords %) \# \.))))

(defn add-offset [offset positions]
  (map #(mapv + offset %) positions))

(defn tile-to-positions
  "Converts a vector of vectors into a sequence of [row col] for those positions that are '#'."
  ([tile-data]
   (let [n-row (count tile-data)
         n-col (count (first tile-data))]
     (for [row (range n-row) col (range n-col) :when (= \# (get-in tile-data [row col]))]
       [row col])))
  ([offset tile-data]
   (add-offset offset (tile-to-positions tile-data))))

(def reorient-to-top
  {:top        identity
   :bottom     #(rotate 180 :flip %)
   :left       #(rotate 90 :flip %)
   :right      #(rotate 90 %)
   :rev-top    #(rotate 0 :flip %)
   :rev-bottom #(rotate 180 %)
   :rev-left   #(rotate 270 %)
   :rev-right  #(rotate 270 :flip %)})

(def reorient-to-left
  {:top        #(rotate 90 :flip %)
   :bottom     #(rotate 270 %)
   :left       identity
   :right      #(rotate 0 :flip %)
   :rev-top    #(rotate 90 %)
   :rev-bottom #(rotate 270 :flip %)
   :rev-left   #(rotate 180 :flip %)
   :rev-right  #(rotate 180 %)})

(defn reorient-tile [tile target matching-edge]
  ((case target
    :top
    (reorient-to-top matching-edge)
    :left
    (reorient-to-left matching-edge)) tile))

(defn solve [data]
  (->> data
       get-corner-tiles
       (map #(% 0))
       (apply *)))

(def sea-monster
  ["..................#."
   "#....##....##....###"
   ".#..#..#..#..#..#..."])

(defn print-full-map [coords]
  (let [[max-r max-c] (map inc (apply map max coords))
        empty-canvas (->> (vec (repeat max-c \.))
                          (repeat max-r)
                          vec)
        canvas
        (reduce (fn [canvas pos] (assoc-in canvas pos \#))
                empty-canvas
                coords)]
    (->> canvas
         (map #(println (apply str %)))
         doall)))

(defn connect-tiles [data]
  (let [top-left (get-top-left-tile data)
        size-in-tiles (int (Math/sqrt (count data)))
        tile-size (->> data first second count)
        positions (rest (for [r (range size-in-tiles)
                              c (range size-in-tiles)]
                          [r c]))
        get-edge-offset (fn [tile-pos] (mapv #(* (dec tile-size) %) tile-pos))
        get-matching-border
        (fn [edge]
          (fn [tile]
            (->> (get-borders tile :both)
                 (map (fn [description border]
                        (if (= border edge) description)) border-order)
                 (filter identity)
                 first)))]
    (loop [[[_ col :as pos] & rem-pos] positions
           rem-tiles (dissoc data (first top-left))
           wave-coords (into #{} (tile-to-positions (second top-left)))]
      (cond
        (nil? pos)
        wave-coords
        :else
        (let [edge-orientation (if (= 0 col) :row :col)
              target-edge-location (if (= 0 col) :top :left)
              matching-edge (edge-from-wave-coords wave-coords (get-edge-offset pos) edge-orientation tile-size)
              get-tile-matching-top (get-matching-border matching-edge)
              [tile-index orientation]
              (->> rem-tiles
                   (map #(update % 1 get-tile-matching-top))
                   (filter #(% 1))
                   first)
              reoriented-tile (reorient-tile (rem-tiles tile-index) target-edge-location orientation)]
          (recur rem-pos
                 (dissoc rem-tiles tile-index)
                 (into wave-coords (tile-to-positions (get-edge-offset pos) reoriented-tile))))))))

(defn count-monsters [monster-coords wave-coords]
  "Assume there are no overlapping monsters"
  (let [normalised-monster-coords
        (mapv #(mapv - % (first monster-coords)) monster-coords)]
    (->> wave-coords
         (map #(add-offset % normalised-monster-coords))
         (filter #(set/subset? % wave-coords))
         count)))

(defn remove-borders [wave-coords tile-size]
  (letfn [(no-border? [pos]
            (every? #(not= 0 (rem % (dec tile-size))) pos))
          (adapt-coord [pos]
            (mapv #(- % (quot % (dec tile-size)) 1) pos))]
    (->> wave-coords
         (filter no-border?)
         (map adapt-coord)
         set)))

(defn solve2 [data]
  (let [wave-coords (remove-borders (connect-tiles data) (count (second (first data))))
        oriented-monster-coords
        (for [angle (range 0 360 90) flip [:flip :no-flip]]
          (set (tile-to-positions (rotate angle flip sea-monster))))
        num-monsters
        (->> oriented-monster-coords
             (map #(count-monsters % wave-coords))
             (filter #(not= 0 %))
             first)]
    (- (count wave-coords)
       (* num-monsters (count (first oriented-monster-coords))))))

(defn run []
  (let [data (parse-data (slurp "resources/input20.txt"))]
    (println (solve data))
    (println (solve2 data))))