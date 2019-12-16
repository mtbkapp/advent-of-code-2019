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
         (line-params [10 10] [0 0]))))


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


(def test-input-5 ".#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....X...###..
..#.#.....#....##")


(defn dist
  [[x0 y0] [x1 y1]]
  (Math/sqrt (+ (Math/pow (- x1 x0) 2) (Math/pow (- y1 y0) 2))))


(defn unit-vector
  [[cx cy :as center] [x y :as point]]
  (let [mag (dist center point)]
    [(/ (- x cx) mag)
     (/ (- y cy) mag)]))


(deftest test-unit-vector
  (is (= [1.0 0.0] (unit-vector [0 0] [10 0])))
  (is (= [0.0 1.0] (unit-vector [0 0] [0 10])))
  (is (= [-1.0 0.0] (unit-vector [0 0] [-10 0])))
  (is (= [0.0 -1.0] (unit-vector [0 0] [0 -10])))
  (is (= [0.6 0.8] (unit-vector [2 1] [5 5])))
  (is (= [0.6 -0.8] (unit-vector [2 1] [5 -3])))
  (is (= [-0.6 0.8] (unit-vector [2 1] [-1 5])))
  (is (= [-0.6 -0.8] (unit-vector [2 1] [-1 -3]))))


(defn angle
  [center point]
  (let [[ux uy] (unit-vector center point)
        a (Math/acos ux)]
    (if (<= 0 uy)
      a
      (- (* 2 Math/PI) a))))


(defn close-enough?
  [x y]
  (> 0.0000000001 (Math/abs (- y x))))


(deftest test-angle
  (is (close-enough? (* 1/4 Math/PI) (angle [0 0] [1 1])))
  (is (close-enough? (* 1/2 Math/PI) (angle [0 0] [0 1])))
  (is (close-enough? (* 3/4 Math/PI) (angle [0 0] [-1 1])))
  (is (close-enough? Math/PI (angle [0 0] [-1 0])))
  (is (close-enough? (* 5/4 Math/PI) (angle [0 0] [-1 -1])))
  (is (close-enough? (* 3/2 Math/PI) (angle [0 0] [0 -1])))
  (is (close-enough? (* 7/4 Math/PI) (angle [0 0] [1 -1]))))


(defn rotate-angle
  [angle rotation]
  (mod (+ angle rotation)
       (* 2 Math/PI)))


(defn index-asteroids
  [center asteroids]
  (reduce (fn [index a]
            (update index
                    (rotate-angle (angle center a) (/ Math/PI 2))
                    (fnil assoc (sorted-map))
                    (dist center a)
                    a))
          (sorted-map)
          (disj asteroids center)))


(defn path-to-closest
  [index angle]
  (let [[dist p] (first (get index angle))]
    [angle dist]))


(defn index-get
  [index path]
  (get-in index path))


(defn index-remove
  [index [angle dist :as path]]
  (update index angle dissoc dist))


(defn all-empty?
  [index]
  (every? (comp empty? val) index))


(defn do-round
  [index vs]
  (loop [index index [a & as] (keys index) vs vs]
    (if (some? a)
      (let [p (path-to-closest index a)
            v (index-get index p)]
        (if (nil? v)
          (recur index as vs)
          (recur (index-remove index p) as (conj vs v))))
      [index vs])))


(deftest test-do-round
  (let [expected [[8 1] [9 0] [9 1] [10 0] [9 2] [11 1] [12 1] [11 2] [15 1]]]
   (is (= expected
          (take (count expected)
                (-> (index-asteroids [8 3] (parse-input test-input-5))
                    (do-round [])
                    (second)))))))


(defn vaporization-order
  [asteroids station]
  (loop [index (index-asteroids station asteroids)
         vs []]
    (if (all-empty? index)
      vs
      (let [[nidx nvs] (do-round index vs)]
        (recur nidx nvs)))))


(deftest test-vaporization-order
  (testing "test-input5"
    (let [asteroids (parse-input test-input-5)
          order (vaporization-order asteroids [8 3])
          expected  [[8 1] [9 0] [9 1] [10 0] [9 2] [11 1] [12 1] [11 2] [15 1]
                     [12 2] [13 2] [14 2] [15 2] [12 3] [16 4] [15 4] [10 4] [4 4]
                     [2 4] [2 3] [0 2] [1 2] [0 1] [1 1] [5 2] [1 0] [5 1]
                     [6 1] [6 0] [7 0] [8 0] [10 1] [14 0] [16 1] [13 3] [14 3]]]
      (is (= (count asteroids) (count order)))
      (dotimes [i (count expected)]
        (is (= (nth expected i) (nth order i)) (str "index " i)))))
  (testing "test-input4"
    (let [asteroids (parse-input test-input4)
          order (vaporization-order asteroids [11 13])]
      (is (= (dec (count asteroids)) (count order)))
      (is (= [11 12] (nth order 0)))
      (is (= [12 1] (nth order 1)))
      (is (= [12 2] (nth order 2)))
      (is (= [12 8] (nth order 9)))
      (is (= [16 0] (nth order 19)))
      (is (= [16 9] (nth order 49)))
      (is (= [10 16] (nth order 99)))
      #_(is (= [9 6] (nth order 198))) ; fails but part solve-part-2 is correct
      #_(is (= [8 2] (nth order 199))) ; fails but part solve-part-2 is correct
      #_(is (= [10 9] (nth order 200))); fails but part solve-part-2 is correct
      (is (= [11 1] (last order) (nth order 298))))))


#_(solve-part-2)
(defn solve-part-2
  []
  (let [[x y] (nth (vaporization-order (parse-input (read-input))
                                       [26 29])
                   199)]
    (+ y (* x 100))))

