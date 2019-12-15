(ns advent-of-code-2019.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer :all]))


(defn line-params
  [[x0 y0] [x1 y1]]
  (let [run (- x1 x0)]
    (if (zero? run)
      {:x x0} 
      (let [m (/ (- y1 y0) run)]
        {:m m 
         :b (- y0 (* m x0))}))))


(deftest test-line-params
  (is (= {:m 1 :b 0}
         (line-params [0 0] [10 10])
         (line-params [10 10] [0 0]))
      
      )
  
  )


(defn read-input
  []
  (slurp (io/resource "day_10.txt")))


(defn parse-input
  [input]
  (let [m (mapv (comp vec seq) (string/split-lines input))]
    (set (for [x (range (count (first m)))
               y (range (count m))
               :when (= \# (get-in m [y x]))]
           [x y]))))


(def test-input0
  ".#..#
.....
#####
....#
...##")


(deftest test-parse-input
  (is (= #{[1 0] [4 0]
           [0 2] [1 2] [2 2] [3 2] [4 2]
           [4 3]
           [3 4] [4 4]}
         (parse-input test-input0))))


(defn unique-lines
  [from to]
  (into #{} 
        (map (partial line-params from))
        to))


(defn group-by-line
  [a0 asteroids]
  (transduce
    (map (juxt identity (partial line-params a0)))
    (completing
      (fn [acc [a1 line]]
        (update acc line (fnil conj #{}) a1)))
    {}
    (disj asteroids a0)))


(defn to-left?
  "Is if p0 to the left p1?"
  [[x0 y0 :as p0] [x1 y1 :as p1]]
  (if (= x0 x1)
    (< y0 y1)
    (< x1 x0)))


(deftest test-to-left?
  (let [center [0 0]
        above [0 1]
        below [0 -1]
        right [1 0]
        left [-1 0]
        above-left [-1 1]
        above-right [1 1]
        below-left [-1 -1]
        below-right [1 -1]]
    (is (to-left? center above))
    (is (not (to-left? center below)))
    (is (not (to-left? center right)))
    (is (to-left? center left))
    (is (to-left? center above-left))
    (is (not (to-left? center above-right)))
    (is (to-left? center below-left))
    (is (not (to-left? center below-right)))))


(defn partition-around
  [a0 on-line]
  (reduce (fn [acc a1]
            (update acc 
                    (if (to-left? a0 a1) :left :right)
                    conj
                    a1))
          {:left #{}
           :right #{}}
          on-line))


(deftest test-to-left?
  (let [center [0 0]
        above [0 1]
        below [0 -1]
        right [1 0]
        left [-1 0]
        above-left [-1 1]
        above-right [1 1]
        below-left [-1 -1]
        below-right [1 -1]]
    (is (= {:left #{above left above-left below-left}
            :right #{below right above-right below-right}}
           (partition-around center [above below
                                     right left
                                     above-left above-right
                                     below-left below-right])))))


(defn count-visible
  [left-right]
  (transduce
    (map (fn [{:keys [left right]}]
           (+ (if (empty? left) 0 1)
              (if (empty? right) 0 1))))
    +
    left-right))


#_(clojure.pprint/pprint
  (count-visible-by-asteroid
  (parse-input test-input0)))    
(defn count-visible-by-asteroid
  [asteroids]
  (into {} 
        (comp
          (map (juxt identity #(group-by-line % asteroids)))
          (map (fn [[a0 lines]]
                 [a0 (map (fn [[line points]]
                            (partition-around a0 points))
                          lines)]))
          (map (juxt first (comp count-visible second))))
        asteroids))


(deftest test-count-visible-by-asteroid
  (is (= {[1 0] 7 [4 0] 7
          [0 2] 6 [1 2] 7 [2 2] 7 [3 2] 7 [4 2] 5
          [4 3] 7
          [3 4] 8
          [4 4] 7}
         (count-visible-by-asteroid (parse-input test-input0)))))


#_(clojure.pprint/pprint (solve-part-1))
(defn solve-part-1
  ([] (solve-part-1 (read-input)))
  ([input]
   (->> (parse-input input)
        (count-visible-by-asteroid)
        (sort-by val)
        (last))))



(def test-input1
  "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####")


(def test-input2
  "#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.")


(def test-input3
  ".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..")


(def test-input4
  ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")


(deftest test-solve-part-1
  (is (= [[3 4] 8] (solve-part-1 test-input0)))
  (is (= [[5 8] 33] (solve-part-1 test-input1)))
  (is (= [[1 2] 35] (solve-part-1 test-input2)))
  (is (= [[6 3] 41] (solve-part-1 test-input3)))
  (is (= [[11 13] 210] (solve-part-1 test-input4))))



(def speeds "Download: 32.97 Mbit/s
Upload: 3.87 Mbit/s
Download: 10.75 Mbit/s
Upload: 2.99 Mbit/s
Download: 13.42 Mbit/s
Upload: 3.76 Mbit/s
Download: 26.90 Mbit/s
Upload: 3.25 Mbit/s
Download: 18.50 Mbit/s
Upload: 3.55 Mbit/s
Download: 19.50 Mbit/s
Upload: 3.90 Mbit/s
Download: 23.62 Mbit/s
Upload: 3.83 Mbit/s
Download: 7.21 Mbit/s
Upload: 3.02 Mbit/s
Download: 13.55 Mbit/s
Upload: 3.30 Mbit/s
Download: 15.47 Mbit/s
Upload: 3.55 Mbit/s
Download: 6.50 Mbit/s
Upload: 3.53 Mbit/s
Download: 8.35 Mbit/s
Upload: 3.09 Mbit/s
Download: 12.51 Mbit/s
Upload: 0.00 Mbit/s
Download: 9.29 Mbit/s
Upload: 2.93 Mbit/s
Download: 17.30 Mbit/s
Upload: 3.49 Mbit/s
Download: 34.12 Mbit/s
Upload: 3.64 Mbit/s
Download: 41.38 Mbit/s
Upload: 3.86 Mbit/s
Download: 43.37 Mbit/s
Upload: 3.96 Mbit/s
Download: 43.53 Mbit/s
Upload: 3.66 Mbit/s
Download: 41.76 Mbit/s
Upload: 3.98 Mbit/s
Download: 41.71 Mbit/s
Upload: 3.89 Mbit/s
Download: 36.83 Mbit/s
Upload: 3.95 Mbit/s
Download: 34.49 Mbit/s
Upload: 3.85 Mbit/s
Download: 33.92 Mbit/s
Upload: 3.91 Mbit/s
Download: 31.31 Mbit/s
Upload: 3.90 Mbit/s
Download: 17.10 Mbit/s
Upload: 2.83 Mbit/s
Download: 36.59 Mbit/s
Upload: 3.72 Mbit/s
Download: 19.48 Mbit/s
Upload: 3.64 Mbit/s
Download: 12.06 Mbit/s
Upload: 3.92 Mbit/s
Download: 21.36 Mbit/s
Upload: 3.26 Mbit/s")



(def sps (->> (string/split-lines speeds)
     (map (comp ffirst (fn [s] (re-seq #"([0-9]*\.[0-9]*)" s))))
     (partition-all 2)
     ))

(def tms (->> (map #(mod % 24) (range 9 100))
              #_(remove zero?)
              (take 30)
              (map #(str (format "%02d" %) ":26"))
              ))

(doseq [[t [down up]] (reverse (map vector tms sps))]
  (println (string/join " " ["2019-12-13" t "-" "Download:" down "Mbit/s" "Upload:" up "Mbit/s"]))
  )

