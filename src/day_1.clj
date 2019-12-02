(ns day-1
  (:require [clojure.string :as str]))


(def input-filename "input/day_1_input")

(def input-list (->> (slurp input-filename)
                     (str/split-lines)
                     (map #(Long. %))))

;; part 1


(defn fuel-requirements
  "Calculate fuel given an amount of mass"
  [mass]
  (- (quot mass 3) 2))

(->> input-list
     (map fuel-requirements)
     (reduce +))

;; part 2

(defn total-fuel-requirements
  "fuel for mass + fuel for fuel"
  [mass]
  (reduce + (take-while pos? (drop 1 (iterate fuel-requirements mass)))))

(->> input-list
     (map total-fuel-requirements)
     (reduce +))
