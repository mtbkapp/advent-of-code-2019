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



(def input-moons #{{:id :io :pos [3 3 0] :v [0 0 0]}
                   {:id :europa :pos [4 -16 2] :v [0 0 0]}
                   {:id :ganymede :pos [-10 -6 5] :v [0 0 0]}
                   {:id :callisto :pos [-3 0 -13] :v [0 0 0]}})


(def test-moons-1 #{{:id :io :pos [-1 0 2] :v [0 0 0]}
                    {:id :europa :pos [2 -10 -7] :v [0 0 0]}
                    {:id :ganymede :pos [4 -8 8] :v [0 0 0]}
                    {:id :callisto :pos [3 5 -1] :v [0 0 0]}})


(def test-moons-2 #{{:id :io :pos [-8 -10 0] :v [0 0 0]}
                    {:id :europa :pos [5 5 10] :v [0 0 0]}
                    {:id :ganymede :pos [2 -7 3] :v [0 0 0]}
                    {:id :callisto :pos [9 -8 -3] :v [0 0 0]}})


; part 1 solution
#_(prn (total-energy (last (tick-n input-moons 1000))))


; part 2, repeated state?

; hints from reddit
; 1. step function is "not surjective" which somehow implies that the first 
;    repeated state is the initial state.
; 2. each dimension can be calculated independently


(def dim-fns
  {:x first
   :y second
   :z #(nth % 2)})


(defn get-moon-dim
  [{:keys [pos v] :as moon} dim]
  (let [f (get dim-fns dim)]
    {:pos (f pos) :v (f v)}))


#_(get-dim (tick test-moons-1) :z)
(defn get-dim
  [moons dim]
  (reduce (fn [r {:keys [id] :as m}]
            (assoc r id (get-moon-dim m dim)))
          {}
          moons))


(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))


(defn lcm
  ([xs] (reduce lcm xs))
  ([a b]
   (* a (/ b (gcd a b)))))


#_(prn (find-repeat-from-initial test-moons-1))
#_(prn (find-repeat-from-initial test-moons-2))
#_(prn (find-repeat-from-initial input-moons))
(defn find-repeat-from-initial 
  [start-moons]
  (let [ix (get-dim start-moons :x)
        iy (get-dim start-moons :y) 
        iz (get-dim start-moons :z)]
    (loop [i 0 
           rx 0 
           ry 0 
           rz 0 
           moons start-moons]
      (if (and (pos? rx) (pos? ry) (pos? rz))
        (lcm [rx ry rz])
        (recur (inc i)
               (if (or (pos? rx) (not= ix (get-dim moons :x))) rx i)
               (if (or (pos? ry) (not= iy (get-dim moons :y))) ry i)
               (if (or (pos? rz) (not= iz (get-dim moons :z))) rz i)
               (tick moons))))))
