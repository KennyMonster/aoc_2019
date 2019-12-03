(ns day_3
  (:require [clojure.string :as str]))

(set! *print-length* 10)

(def input-filename "input/day_3_input")

(defn parse-path
  "first letter is char, remaining is long"
  [ds]
  (let [direction-char (first ds)
        int-str        (subs ds 1)]
    [direction-char (Long. int-str)]))

;(parse-direction "R233")

(defn str->paths [s]
  (-> s
      (str/split #",")
      (->> (map parse-path))))

(def input-paths
  (->>
    (slurp input-filename)
    (str/trim)
    (str/split-lines)
    (map str->paths)))


(defn get-end-pos
  "Given a starting pos, find the end pos"
  [pos path]
  (let [[x y] pos
        [dir mag] path]
    (case dir
      \U [x (+ y mag)]
      \D [x (- y mag)]
      \R [(+ x mag) y]
      \L [(- x mag) y])))

;(get-end-pos [0 0] [\U 1])


(defn get-pos-seq
  "Given a starting pos and path, return a seq
  of all positions between start and end"
  [pos path]
  (let [[x0 y0] pos
        [dir mag] path
        delta (inc mag)]
    (case dir
      \U (for [y (range (inc y0) delta 1)] [x0 y])
      \D (for [y (range (dec y0) (- y0 delta) -1)] [x0 y])
      \R (for [x (range (inc x0) delta 1)] [x y0])
      \L (for [x (range (dec x0) (- x0 delta) -1)] [x y0]))))

;(get-pos-seq [0 0] [\L 3])


(defn wire-positions
  "Find all positions occupied by a wire specified by paths"
  [paths]
  (loop [paths     paths
         positions #{}
         curr-pos  [0 0]]
    (if (seq paths)
      (let [path              (first paths)
            end-pos           (get-end-pos curr-pos path)
            updated-positions (into positions (get-pos-seq curr-pos path))]
        (recur (rest paths)
               updated-positions
               end-pos))

      positions)))

;(wire-pos (str->paths "L3"))

(defn manhattan-distance
  "distance from 0 0 to pos"
  [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(time
  (->> input-paths
       (map wire-positions)
       (apply clojure.set/intersection)
       (apply min-key manhattan-distance)
       (manhattan-distance)))


(def t1 ["R8,U5,L5,D3"
         "U7,R6,D4,L4"])

#_(->> t1
       (map str->paths)
       (map wire-positions)
       (apply clojure.set/intersection))


;(wire-positions (str->paths "R3,U3,L3,D3"))