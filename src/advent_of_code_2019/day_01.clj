(ns advent-of-code-2019.day-01
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]))



(defn fuel-for-mass
  [mass]
  (Math/max
    (-> (/ mass 3.0)
        (Math/floor)
        (int)
        (- 2))
    0))


(deftest test-fuel-for-mass
  (is (= 0 (fuel-for-mass 0)))
  (is (= 0 (fuel-for-mass 1)))
  (is (= 1 (fuel-for-mass 9)))
  (is (= 2 (fuel-for-mass 12)))
  (is (= 2 (fuel-for-mass 14)))
  (is (= 654 (fuel-for-mass 1969)))
  (is (= 33583 (fuel-for-mass 100756))))


(defn read-input
  []
  (with-open [rdr (io/reader (io/resource "day_01_part1.txt"))]
    (doall (line-seq rdr))))


#_(prn (solve-part-1))
(defn solve-part-1
  []
  (transduce
    (comp (map #(Long/valueOf %))
          (map fuel-for-mass))
    +
    0
    (read-input)))


(defn real-fuel-for-mass
  [mass]
  (if (<= mass 0)
    0
    (let [ffm (fuel-for-mass mass)]
      (+ ffm (real-fuel-for-mass ffm)))))


(deftest test-real-fuel-for-mass
  (is (= 0 (real-fuel-for-mass 0)))
  (is (= 0 (real-fuel-for-mass 1)))
  (is (= 1 (real-fuel-for-mass 9)))
  (is (= 2 (real-fuel-for-mass 12)))
  (is (= 2 (real-fuel-for-mass 14)))
  (is (= 966 (real-fuel-for-mass 1969)))
  (is (= 50346 (real-fuel-for-mass 100756))))


#_(prn (solve-part-2))
(defn solve-part-2
  []
  (transduce
    (comp (map #(Long/valueOf %))
          (map real-fuel-for-mass))
    +
    0
    (read-input)))

