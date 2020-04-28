(ns advent-of-code-2019.day-17
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [advent-of-code-2019.intcode :as intcode]))


(defn get-photo
  []
  (->> (intcode/read-program "day_17.txt")
       (intcode/new-computer)
       (intcode/wrap-collect-output)
       (map char)))


(def test-photo (seq "..#..........
..#..........
#######...###
#.#...#...#.#
#############
..#...#...#..
..#####...^.."))


(defn get-scaffolding-points
  [photo]
  (-> (reduce (fn [{:keys [x y] :as s} chr]
                (if (= \newline chr)
                  (-> s (assoc :x 0) (update :y inc))
                  (cond-> (update s :x inc)
                    (not= \. chr) (update :ps conj [x y]))))
              {:x 0 :y 0 :ps #{}}
              photo)
      :ps))


#_(= #{[2 3] [2 1] [3 2] [1 2]} (set (adj-points [2 2])))
(defn adj-points
  [[x y]]
  (for [dx #{-1 0 1}
        dy #{-1 0 1}
        :when (and (or (zero? dx) (zero? dy))
                   (not= [0 0] [dx dy]))]
    [(+ x dx) (+ y dy)]))


(defn get-intersections
  [ps]
  (filter (fn [p]
            (every? #(contains? ps %)
                    (adj-points p)))
          ps))


#_(get-alignment-sum test-photo)
#_(prn (get-alignment-sum (get-photo))) ; part 1 = 3888
(defn get-alignment-sum
  [photo]
  (->> (get-scaffolding-points photo)
       (get-intersections)
       (map (fn [[x y]] (* x y)))
       (reduce +)))


