(ns day_3
  (:require [clojure.string :as str]))

(set! *print-length* 30)

(defn trace [x] (do (println x) x))

(def input-filename "input/day_3_input")

(defn parse-path
  "first letter is char, remaining is long"
  [ds]
  (let [direction-char (first ds)
        int-str        (subs ds 1)]
    [direction-char (Long. int-str)]))

;(parse-path "R233")

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
  "Given a starting pos and path, find the end pos"
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
      \U (for [y (range (inc y0) (+ y0 delta) 1)] [x0 y])
      \D (for [y (range (dec y0) (- y0 delta) -1)] [x0 y])
      \R (for [x (range (inc x0) (+ x0 delta) 1)] [x y0])
      \L (for [x (range (dec x0) (- x0 delta) -1)] [x y0]))))

;(get-pos-seq [0 0] [\L 3])


(defn wire-positions
  "Find all positions occupied by a wire specified by paths"
  [paths]
  (loop [paths     paths
         positions []
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


(defn intersections
  "Where 2 wires intersect. Input is seq of wire paths"
  [wire-paths]
  (->> wire-paths
       (map wire-positions)
       (map set)
       (apply clojure.set/intersection)))

(time
  (->> (intersections input-paths)
       (apply min-key manhattan-distance)
       (manhattan-distance)))

;; Part 2


(defn steps-to-intersections
  "Given the path positions in order of a wire
  and a set of all intersections, return a map
  of intersection pos -> shorted number of steps"
  [path-positions intersections-set]
  (loop [path-positions       path-positions
         intersect-pos->steps {}
         steps-walked         1] ;; [0 0] start not part of path-positions
    (if (seq path-positions)
      (let [curr-pos       (first path-positions)
            rest-positions (rest path-positions)]
        ; if an intersection point, add to map, otherwise just keep walking
        (if (and (intersections-set curr-pos)
                 (not (contains? intersect-pos->steps curr-pos)))
          (recur rest-positions (assoc intersect-pos->steps curr-pos steps-walked) (inc steps-walked))
          (recur rest-positions intersect-pos->steps (inc steps-walked))))

      intersect-pos->steps)))


(time
  (let [intersect-positions (intersections input-paths)
        step-maps           (map #(steps-to-intersections % intersect-positions) (map wire-positions input-paths))
        merged-step-maps    (apply merge-with vector step-maps)
        total-distances     (map #(apply + %) (vals merged-step-maps))]
    (reduce min total-distances)))
