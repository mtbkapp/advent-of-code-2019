(ns advent-of-code-2019.day-03
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.test :refer :all]))



(defn parse-input
  [input]
  (map (comp #(map (juxt first
                         (fn [s] (Long/valueOf (subs s 1))))
                   %)
             #(string/split % #","))
       (string/split-lines input)))


(deftest test-parse-input
  (is (= [[[\R 8] [\U 5] [\L 5] [\D 3]]
          [[\U 7] [\R 6] [\D 4] [\L 4]]]
         (parse-input "R8,U5,L5,D3\nU7,R6,D4,L4"))))


(defn read-input
  []
  (slurp (io/resource "day_03.txt")))


(defn vec-add
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])


(defn scale-vec
  [[x y] s]
  [(* x s) (* y s)])


(def dir->unit-vec
  {\R [1 0]
   \L [-1 0]
   \U [0 1]
   \D [0 -1]})


(defn make-line
  [pos [dir cnt]]
  (map (fn [s] 
         (vec-add pos (scale-vec (dir->unit-vec dir) s)))
       (range 1 (inc cnt))))


(defn rasterize-wire
  [wire]
  (first (reduce (fn [[points pos] move]
                   (let [line (make-line pos move)]
                     [(into points line) (last line)]))
                 [#{} [0 0]]
                 wire)))


(defn mdist
  [[x y]]
  (+ (Math/abs x) (Math/abs y)))


#_(solve-part-1)
(defn solve-part-1
  ([] (solve-part-1 (parse-input (read-input))))
  ([wires]
   (->> wires 
       (map rasterize-wire)
       (apply sets/intersection)
       (map mdist)
       (reduce min))))


(deftest test-closest-intersection
  (is (= 6 (solve-part-1 (parse-input "R8,U5,L5,D3\nU7,R6,D4,L4"))))
  (is (= 159 (solve-part-1 (parse-input "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"))))
  (is (= 135 (solve-part-1 (parse-input "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))))
