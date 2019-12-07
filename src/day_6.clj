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


