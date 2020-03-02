(ns advent-of-code-2019.day-03-try2
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.test :refer :all]))


(def dir-vecs
  {\R [1 0]
   \L [-1 0]
   \U [0 1]
   \D [0 -1]})


(defn vec-add
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])


(defn trace-dir
  ([steps [dir cnt]] (trace-dir steps (last steps) (dir-vecs dir) cnt))
  ([steps last-step unit-vec cnt]
   (if (<= cnt 0)
     steps
     (let [next-step (vec-add last-step unit-vec)]
       (recur (conj steps next-step)
              next-step
              unit-vec
              (dec cnt))))))


(deftest test-trace-dir
  (is (= [[0 0] [1 0]] (trace-dir [[0 0]] [\R 1])))
  (is (= [[0 0] [1 0] [2 0]] (trace-dir [[0 0]] [\R 2])))
  (is (= [[0 0]] (trace-dir [[0 0]] [\R 0])))
  (is (= [[0 0]] (trace-dir [[0 0]] [\R -1])))
  (is (= [[0 0] [0 -1] [0 -2]] (trace-dir [[0 0]] [\D 2]))))


(defn trace-wire
  [wire]
  (reduce trace-dir [[0 0]] wire))


(deftest test-trace-wire
  (is (= [[0 0] [1 0] [1 1] [0 1] [0 0]]
         (trace-wire [[\R 1] [\U 1] [\L 1] [\D 1]])))
  (is (= [[0 0]
          [1 0] [2 0]
          [2 1] [2 2]
          [1 2] [0 2]
          [0 1] [0 0]]
         (trace-wire [[\R 2]
                      [\U 2]
                      [\L 2]
                      [\D 2]]))))


(defn parse-wire
  [line]
  (map (juxt first #(Long/valueOf (subs % 1)))
       (string/split line #",")))


(defn parse-input
  [input]
  (map parse-wire (string/split-lines input)))


(deftest test-parse-input
  (is (= [[[\R 8] [\U 5] [\L 5] [\D 333]]
          [[\U 7] [\R 6] [\D 4] [\L 4]]]
         (parse-input "R8,U5,L5,D333\nU7,R6,D4,L4"))))


(defn find-intersections
  [[w0 w1]]
  (-> (sets/intersection (set (trace-wire w0))
                         (set (trace-wire w1)))
      (disj [0 0])))


(defn mdist 
  [[x y]]
  (+ (Math/abs x) (Math/abs y)))


(defn closest-intersection
  [wires]
  (->> (find-intersections wires)
       (map mdist)
       (reduce min)))


(deftest test-closest-intersection
  (is (= 6 (closest-intersection (parse-input "R8,U5,L5,D3\nU7,R6,D4,L4"))))
  (is (= 159 (closest-intersection (parse-input "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"))))
  (is (= 135 (closest-intersection (parse-input "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))))


; answer 1064
(defn solve-part-1
  []
  (closest-intersection (parse-input (slurp (io/resource "day_03.txt")))))


(defn find-dists 
  [trace]
  (second
    (reduce (fn [[d ds] pos]
              (if-let [d (get ds pos)]
                [(inc d) ds]; use min or first?
                [(inc d) (assoc ds pos d)]))
            [0 {}]
            trace)))

(deftest test-find-dists
  (is (= {[0 0] 0
          [1 0] 1
          [2 0] 2
          [2 1] 3
          [1 1] 4
          [1 -1] 2}
         (find-dists [[0 0] [1 0] [2 0] [2 1] [1 1] [1 0] [1 -1]]))))


(comment
  (def wires (parse-input (slurp (io/resource "day_03.txt"))))
  (def w0 (first wires))
  (def w1 (second wires))

  (def tw0 (trace-wire w0))
  (def tw1 (trace-wire w1))

  (def dw0 (find-dists tw0))
  (def dw1 (find-dists tw1))

  (def inter (find-intersections wires))
  (count inter)
  (prn inter)
  

  (every? #(contains? dw0 %) inter)
  (every? #(contains? dw1 %) inter)

  (def ddw0 (select-keys dw0 inter))
  (def ddw1 (select-keys dw1 inter))

  (def w0-short (first (sort-by val ddw0)))
  (def w1-short (first (sort-by val ddw1)))

  (prn [w0-short w1-short])
  (+ 10289 1831) ; 12120, invalid by logic

 
  (prn (sort (map (fn [p] (+ (dw0 p) (dw1 p))) inter)))

  ; 14260, not sure how



  )



