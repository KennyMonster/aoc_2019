(ns day-6-test
  (:require [clojure.test :refer :all]
            [day-6 :refer :all]))




;(deftest test-ret6
;  (is (= 6 (ret6))))

(deftest test-parse-input
  (is (= (parse-input test-input)
         {"K" "J", "L" "K", "G" "B", "J" "E", "H" "G", "E" "D", "C" "B", "F" "E", "B" "COM", "I" "D", "D" "C"})))