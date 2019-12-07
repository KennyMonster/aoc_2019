(ns day-6
  (:require [clojure.string :as str]))


(def input-filename "input/day_6_input")

(defn parse-input [s]
  (->> s
       (str/split-lines)
       (map #(str/split % #"\)"))
       (map reverse)
       (map vec)
       (into {})))

;(parse-input (slurp input-filename))

(def test-input
  "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L")

(def parent-map (parse-input (slurp input-filename)))

(defn path-to-root [parent-map id]
  (loop [id    id
         path  []]
    (let [parent (get parent-map id)]
      (if (nil? parent)
        path
        (recur parent (conj path parent))))))

;(path-to-root parent-map "CO")

(->> (keys parent-map)
     (map #(count (path-to-root parent-map %)))
     (reduce +))