(ns day_2
  (:require [clojure.string :as str]))

(def input-filename "input/day_2_input")

(def input-vec
  (->
    (slurp input-filename)
    (str/trim)
    (str/split #",")
    (->> (map #(Long. %)))
    (vec)))

(def restored-state
  (->
    input-vec
    (assoc 1 12)
    (assoc 2 2)))

(defn exec
  "Take a program and program counter and return
  the program contents after 1 cycle"
  [program pc]
  (let [[op src1 src2 dest] (subvec program pc)
        exec-op #(assoc program dest (% (nth program src1) (nth program src2)))]
    (case op
      1 (exec-op +)
      2 (exec-op *)
      99 nil
      (throw (Exception. (str "bad instruction " op))))))

(defn run
  "Run a program til completion"
  [program pc]
  (if-let [new-state (exec program pc)]
    (recur new-state (+ pc 4))
    program))


(first (run restored-state 0))