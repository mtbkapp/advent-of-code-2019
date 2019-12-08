(ns advent-of-code-2019.day-06
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [loom.alg :as galg]
            [loom.graph :as graph]))


(defn read-input
  []
  (slurp (io/resource "day_06.txt")))


(defn parse-input
  [input]
  (transduce 
    (map #(string/split % #"\)"))
    (completing
      (fn [graph [dest src]]
        (assoc graph src dest)))
    {}
    (string/split-lines input)))


(defn count-paths-from
  ([graph node] (count-paths-from graph node 0))
  ([graph node cnt]
   (if-let [next-node (get graph node)]
     (recur graph next-node (inc cnt))
     cnt)))


(defn count-all-paths
  [graph]
  (transduce
    (map #(count-paths-from graph %))
    +
    (keys graph)))


(def test-orbits 
  "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L") 

(deftest test-count-all-paths
  (is (= 42 (count-all-paths (parse-input test-orbits)))))

#_(prn (solve-part-1))
(defn solve-part-1
  []
  (count-all-paths (parse-input (read-input))))


(def test-orbits2
  "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN")


(defn add-adj-node
  [adj-map src dst]
  (update adj-map src (fnil conj #{}) dst))


#_(build-adj-map (parse-input test-orbits2))
(defn build-adj-map
  [input]
  (reduce (fn [adj-map [src dst]]
            (-> adj-map
                (add-adj-node src dst)
                (add-adj-node dst src)))
          {}
          input))


#_(solve-part-2-with-loom)
#_(solve-part-2-with-loom (parse-input test-orbits2))
(defn solve-part-2-with-loom
  ([] (solve-part-2-with-loom (parse-input (read-input))))
  ([input]
   (-> (apply graph/weighted-graph input)
       (galg/dijkstra-path-dist "YOU" "SAN")
       (second)
       (- 2))))


