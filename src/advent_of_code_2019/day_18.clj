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


(def ex3 "########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################")


(def ex4 "#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################")


(defn read-cell
  [cell]
  (cond (= cell \#) :wall
        (= cell \.) :space
        (= cell \@) :start
        (Character/isLowerCase cell) (keyword "key" (str cell))
        (Character/isUpperCase cell) (keyword "door" (string/lower-case (str cell)))
        :else (throw (IllegalStateException. (str "Invalid cell: " cell)))))


(defn find-start
  [maze w h]
  (->> (for [y (range h)
             x (range w)]
         [x y])
       (drop-while (fn [[x y]]
                     (not= :start (get-in maze [y x]))))
       (first)))


(defn init
  [maze]
  (let [h (count maze)
        w (count (first maze))]
    {:maze maze
     :size [w h]
     :start (find-start maze w h)}))


(defn read-input
  ([] (read-input (slurp (io/resource "day_18.txt"))))
  ([input]
   (init (mapv (partial mapv read-cell) (string/split-lines input)))))


(defn ns?
  [kw n]
  (and (keyword? kw) (= n (namespace kw))))

(defn key?
  [kw]
  (ns? kw "key"))

(defn door?
  [kw]
  (ns? kw "door"))


(deftest test-key?-and-door?
  (is (key? :key/a))
  (is (not (key? :door/a)))
  (is (not (key? :start)))
  (is (not (key? nil))))


(defn key->door
  [k]
  (keyword "door" (name k)))

(defn door->key 
  [d]
  (keyword "key" (name d)))


(def up [0 -1])
(def down [0 1])
(def right [1 0])
(def left [-1 0])

(defn adjacent
  [maze [x y ks :as pos]]
  (reduce (fn [adj [dx dy]]
            (let [nx (+ x dx)
                  ny (+ y dy)
                  c (get-in maze [ny nx])]
              (cond ; it's a key, pick it up.
                    (key? c)
                    (conj adj [nx ny (conj ks c)])
                    ; it's a door and we have the key.
                    (and (door? c) (contains? ks (door->key c)))
                    (conj adj [nx ny ks])
                    ; it's a space
                    (#{:start :space} c)
                    (conj adj [nx ny ks])
                    ; off the maze, a wall, or a door we don't have the key for 
                    :else
                    adj)))
          #{} 
          [up down left right]))


(deftest test-adjacent
  (let [{:keys [maze]} (read-input ex1)]
    (testing "can move to spaces but not walls"
      (is (= #{[4 1 #{}] [6 1 #{}]} (adjacent maze [5 1 #{}]))))
    (testing "moving to a cell with a key puts the key in key set"
      (is (= #{[5 1 #{}] [7 1 #{:key/a}]} (adjacent maze [6 1 #{}]))))
    (testing "moving to a key which has already been obtained"
      (is (= #{[5 1 #{:key/a}] [7 1 #{:key/a}]} (adjacent maze [6 1 #{:key/a}]))))
    (testing "can move to cell with door if key is in key set"
      (is (= #{[5 1 #{}]} (adjacent maze [4 1 #{}])))
      (is (= #{[3 1 #{:key/a}] [5 1 #{:key/a}]} (adjacent maze [4 1 #{:key/a}]))))))



; goal is to find the size of the shortest path from
; [start-x start-y #{]] to [_ _ every-key-in-maze?] 
; 
