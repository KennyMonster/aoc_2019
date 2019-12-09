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
  (loop [id   id
         path []]
    (let [parent (get parent-map id)]
      (if (nil? parent)
        path
        (recur parent (conj path parent))))))

;(path-to-root parent-map "CO")

(->> (keys parent-map)
     (map #(count (path-to-root parent-map %)))
     (reduce +))


;; part 2

(def test-input-2
  "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN")


(defn add-edge-to-adjacency-list
  "Given a map and tuple of nodes, updates AJ"
  [adj-list [v1 v2]]
  (let [update-adj-list (fn [adj-list v1 v2]
                          (update adj-list v1 (fnil #(conj % v2) #{})))]
    (-> adj-list
        (update-adj-list v1 v2)
        (update-adj-list v2 v1))))

#_(-> {}
      (add-edge-to-adjacency-list ["a" "b"])
      (add-edge-to-adjacency-list ["a" "c"])
      (add-edge-to-adjacency-list ["b" "d"]))


(defn build-adjacency-list
  "Given an input string, return an adjacency list map
  key is node and value is set of adjacenct nodes"
  [s]
  (->> s
       (str/split-lines)
       (map #(str/split % #"\)"))
       (reduce add-edge-to-adjacency-list {})))

;(build-adjacency-list test-input)


(defn dijkstra
  "Single source shortest paths to all ndoes"
  [adj-list source-node]
  (let [initial-dist (into {} (map #(vector % ##Inf) (keys adj-list)))]
    (loop [; queue of nodes to visit in BFS
           queue    [source-node]

           ; distances from source node to every other node in graph
           dist     (assoc initial-dist source-node 0)

           ; set of nodes already visisted
           visisted #{}]
      (if (seq queue)
        (let [curr-v        (first queue)
              alt-dist      (inc (get dist curr-v))
              neighbors     (get adj-list curr-v)
              ; update diststances to neighbors if shorter than current path
              updated-dist  (reduce (fn [dist neighbor-v]
                                      (update dist neighbor-v min alt-dist))
                                    dist
                                    neighbors)
              updated-queue (concat (rest queue) (clojure.set/difference neighbors visisted))]
          (recur updated-queue
                 updated-dist
                 (conj visisted curr-v)))

        dist))))

(dijkstra (build-adjacency-list test-input-2) "YOU")

(time
  ((dijkstra (build-adjacency-list (slurp input-filename)) "YOU") "SAN"))

; 438 is too high?

; it's just 436 because YOU and SAN are pretend nodes, so subtract them out