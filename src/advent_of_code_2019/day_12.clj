(ns advent-of-code-2019.day-12
  (:require [clojure.test :refer :all]))



(defn vec-add
  [& vs]
  (apply mapv (fn [a b] (+ a b)) vs))


(defn delta-v
  [moon-pos neighboor-pos]
  (mapv (fn [a b]
          (compare b a))
        moon-pos 
        neighboor-pos))


(defn update-velocity
  [{mp :pos :as moon} {np :pos :as neighboor}]
  (update moon :v vec-add (delta-v mp np)))


(defn update-velocities
  [moons]
  (into #{}
        (map (fn [m]
               (reduce update-velocity m (disj moons m))))
        moons))


(deftest test-update-velocities
  (let [input #{{:id 0 :pos [-1 0 2] :v [0 0 0]}
                {:id 1 :pos [2 -10 -7] :v [0 0 0]}
                {:id 2 :pos [4 -8 8] :v [0 0 0]}
                {:id 3 :pos [3 5 -1] :v [0 0 0]}}
        expected  [{:id 0 :pos [-1 0 2] :v [3 -1 -1]}
                   {:id 1 :pos [2 -10 -7] :v [1 3 3]}
                   {:id 2 :pos [4 -8 8] :v [-3 1 -3]}
                   {:id 3 :pos [3 5 -1] :v [-1 -3 1]}]
        actual (sort-by :id (update-velocities input))]
    (doseq [i (range (count expected))]
      (is (= (nth expected i)
             (nth actual i))))))


(defn update-positions
  [moons]
  (into #{}
        (map (fn [m]
               (update m :pos vec-add (:v m))))
        moons))


(defn tick
  [moons]
  (update-positions (update-velocities moons)))


(deftest test-tick
  (let [input #{{:id 0 :pos [-1 0 2] :v [0 0 0]}
                {:id 1 :pos [2 -10 -7] :v [0 0 0]}
                {:id 2 :pos [4 -8 8] :v [0 0 0]}
                {:id 3 :pos [3 5 -1] :v [0 0 0]}}
        expected  [{:id 0 :pos [2 -1 1] :v [3 -1 -1]}
                   {:id 1 :pos [3 -7 -4] :v [1 3 3]}
                   {:id 2 :pos [1 -7 5] :v [-3 1 -3]}
                   {:id 3 :pos [2 2 0] :v [-1 -3 1]}]
        actual (sort-by :id (tick input))]
    (doseq [i (range (count expected))]
      (is (= (nth expected i)
             (nth actual i))))))


(defn tick-n
  [moons n]
  (take (inc n) (iterate tick moons)))


(deftest test-tick-n
  (let [input #{{:id 0 :pos [-1 0 2] :v [0 0 0]}
                {:id 1 :pos [2 -10 -7] :v [0 0 0]}
                {:id 2 :pos [4 -8 8] :v [0 0 0]}
                {:id 3 :pos [3 5 -1] :v [0 0 0]}}
        expected  [{:id 0 :pos [2 1 -3] :v [-3 -2 1]}
                   {:id 1 :pos [1 -8 0] :v [-1 1 3]}
                   {:id 2 :pos [3 -6 1] :v [3 2 -3]}
                   {:id 3 :pos [2 0 4] :v [1 -1 -1]}]
        steps (tick-n input 10)
        actual (sort-by :id (last steps))]
    (is (= input (first steps)))
    (doseq [i (range (count expected))]
      (is (= (nth expected i)
             (nth actual i))))))

(defn sum-abs
  [xs]
  (transduce (map #(Math/abs %)) + xs))


(defn total-energy
  [moons]
  (transduce (map (fn [{:keys [pos v]}]
                    (* (sum-abs pos)
                       (sum-abs v))))
             +
             moons))


(deftest test-total-energy
  (is (= 179 (total-energy [{:id 0 :pos [2 1 -3] :v [-3 -2 1]}
                            {:id 1 :pos [1 -8 0] :v [-1 1 3]}
                            {:id 2 :pos [3 -6 1] :v [3 2 -3]}
                            {:id 3 :pos [2 0 4] :v [1 -1 -1]}]))))


(deftest test-tick+total-energy
  (let [input #{{:id 0 :pos [-8 -10 0] :v [0 0 0]}
                {:id 1 :pos [5 5 10] :v [0 0 0]}
                {:id 2 :pos [2 -7 3] :v [0 0 0]}
                {:id 3 :pos [9 -8 -3] :v [0 0 0]}}
        expected  [{:id 0 :pos [8 -12 -9] :v [-7 3 0]}
                   {:id 1 :pos [13 16 -3] :v [3 -11 -5]}
                   {:id 2 :pos [-29 -11 -1] :v [-3 7 4]}
                   {:id 3 :pos [16 -13 23] :v [7 1 1]}]
        steps (tick-n input 100)
        actual (sort-by :id (last steps))]
    (doseq [i (range (count expected))]
      (is (= (nth expected i)
             (nth actual i))))
    (is (= 1940 (total-energy (last steps))))))



(def input-moons #{{:pos [3 3 0] :v [0 0 0]}
                   {:pos [4 -16 2] :v [0 0 0]}
                   {:pos [-10 -6 5] :v [0 0 0]}
                   {:pos [-3 0 -13] :v [0 0 0]}})




#_(time (ticks-till-repeat #{{:pos [-1 0 2] :v [0 0 0]} {:pos [2 -10 -7] :v [0 0 0]} {:pos [4 -8 8] :v [0 0 0]} {:pos [3 5 -1] :v [0 0 0]}}))
(defn ticks-till-repeat
  [start]
  (loop [seen #{} ticks 0 state start]
    (if (contains? seen state)
      ticks
      (recur (conj seen state)
             (inc ticks)
             (tick state)))))


(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))


(defn lcm
  ([xs] (reduce lcm xs))
  ([a b]
   (/ (Math/abs (* a b))
      (gcd a b))))









