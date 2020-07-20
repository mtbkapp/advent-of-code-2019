(ns advent-of-code-2019.day-18
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as sets]
            [clojure.test :refer :all]))


(def ex1 "#########
#b.A.@.a#
#########")


(def ex2 "########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################")


(defn parse-block
  [c]
  (cond (= \# c) :wall
        (= \@ c) :start
        (= \. c) :space
        (Character/isLowerCase c) (keyword "key" (str c))
        (Character/isUpperCase c) (keyword "door" (string/lower-case (str c)))
        :else (throw (ex-info "Unexpected input" {:char c}))))


(defn find-start
  ([input] (find-start input 0 0))
  ([{:keys [m width height] :as input} x y]
   (cond (= height y) nil
         (= width x) (recur input 0 (inc y))
         (= :start (get-in m [y x])) [x y]
         :else (recur input (inc x) y))))


(defn assoc-m
  "Update the cell at [x y] with c in input."
  [input [x y] c]
  (assoc-in input [:m y x] c))


(defn add-start
  [input]
  (let [start (find-start input)]
    (-> (assoc input :start start)
        (assoc-m start :space))))


(defn index-doors
  [{:keys [m width height] :as input}]
  (assoc input
         :doors
         (into {}
               (for [y (range height)
                     x (range width)
                     :let [c (get-in m [y x])]
                     :when (= "door" (namespace c))]
                 [c [x y]]))))


(defn read-input
  ([] (read-input (slurp (io/resource "day_18.txt"))))
  ([input]
   (let [m (mapv (partial mapv parse-block) (string/split-lines input))]
     (-> {:m m
          :width (count (first m))
          :height (count m)}
         (add-start)
         (index-doors)))))


(deftest test-read-input
  (let [{:keys [m width height start doors] :as input} (read-input ex1)]
    (is (= #{:m :width :height :start :doors} (set (keys input))))
    (is (= [5 1] start))
    (is (= 9 width))
    (is (= 3 height))
    (is (= {:door/a [3 1]} doors))
    (is (vector? m))
    (is (every? vector? m))
    (is (= (repeat 9 :wall) (first m)))
    (is (= (repeat 9 :wall) (last m)))
    (is (= [:wall :key/b :space :door/a :space :space :space :key/a :wall]
           (second m))))
  (let [{:keys [m width height start doors] :as input} (read-input)]
    (is (= [40 40] start))
    (is (= 81 width height))
    (is (map? doors))
    (is (not (contains? (into #{} (mapcat identity) m) :start)))))


(defn key->door
  "Given a key keyword returns it's corresponding door keyword."
  [k]
  (keyword "door" (name k)))


(defn get-adjacent
  [{:keys [m] :as input} [x y :as p]]
  (reduce (fn [r [[x y] c]]
            (update r c #(conj % [x y])))
          {}
          (for [dx [-1 0 1]
                dy [-1 0 1]
                :let [nx (+ x dx)
                      ny (+ y dy)
                      c (get-in m [ny nx])]
                :when (and (not (= p [nx ny]))
                           (some? c)
                           (not= c :wall))]
            [[nx ny] c])))


(defn merge-doors-and-keys
  [r adj dist]
  (reduce (fn [nr [k [p]]]
            (let [[_ curr-dist] (get nr k)]
              (if (and (some? curr-dist) (<= curr-dist dist))
                nr
                (assoc nr k [p dist]))))
          r
          (dissoc adj :wall :space :start)))

#_(explore-from (read-input ex1) [5 1])
(defn explore-from
  "Starting at [x y] return a mapping from doors/keys to the coordinate of the
  door/key and the min number of steps to get there."
  [input start]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY [start 0])
         visited #{}
         r {}]
    (if (empty? q)
      r
      (let [[[x y] dist] (peek q) 
            adj (get-adjacent input [x y])]
        (recur (into (pop q)
                     (map #(vector % (inc dist)))
                     (sets/difference (set (:space adj)) visited))
               (conj visited [x y])
               (merge-doors-and-keys r adj (inc dist)))))))


; this function doesn't help
(defn find-min-key
  [r]
  (transduce (filter (comp (partial = "key") namespace key))
             (completing
               (fn [[_ [_ min-dist] :as min-k] [_ [_ dist] :as x]]
                 (cond (nil? min-k) x
                       (< dist min-dist) x
                       :else min-k)))
             nil
             r))


; greedy algorithm
; find all keys from given location
; choose the closest key 
; remove the door corresponding to the chosen key 
; move to chosen key location
; repeat until there are no more keys, record dist

; will this result in the minimum number of ? no -> ex2


; brute force
; build a tree where there is a branch at every choice of keyA
; tree is rooted at nil key
; nodes are a key and the state after choosing that key
; leafs are dist to recover all keys
; so traversal from root to any leaf will give sequence of choices and cost of
;   of that sequence


; heuristic to choose best possible key?

#_(prn (part-1 (read-input ex1)))
#_(prn (part-1 (read-input ex2)))
(defn part-1
  ([{:keys [start] :as input}]
   (part-1 input (explore-from input start) 0))
  ([{:keys [doors] :as input} r total-dist]
   (if-let [[k [k-coord k-dist]] (find-min-key r)]
     (let [door-coord (get doors (key->door k))
           next-input (cond-> (assoc-m input k-coord :space)
                        (some? door-coord) (assoc-m door-coord :space))]
       (recur next-input
              (explore-from next-input k-coord)
              (+ total-dist k-dist)))
     total-dist)))



; todo

; 1 redo explore-from to only return keys, their distances, and their coordinates
; 2 write test for explore-from
; 3 write fn to build decision tree, probably recursive



