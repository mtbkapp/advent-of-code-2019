(ns advent-of-code-2019.day-18
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as sets]
            [clojure.test :refer :all])
  (:import [java.util HashMap HashSet PriorityQueue]))


(set! *warn-on-reflection* true)


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

(def ex5 "########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################")


(defn read-cell
  [^Character cell]
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


(defn ns?
  [kw n]
  (and (keyword? kw) (= n (namespace kw))))

(defn key?
  [kw]
  (ns? kw "key"))

(defn door?
  [kw]
  (ns? kw "door"))


(defn find-all-keys
  [maze]
  (into #{}
        (comp (mapcat identity)
              (filter key?))
        maze))


(defn init
  [maze]
  (let [h (count maze)
        w (count (first maze))]
    {:maze maze
     :size [w h]
     :start (find-start maze w h)
     :all-keys (find-all-keys maze)}))


(defn read-input
  ([] (read-input (slurp (io/resource "day_18.txt"))))
  ([input]
   (init (mapv (partial mapv read-cell) (string/split-lines input)))))


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

; got hint from https://medium.com/@werner.altewischer/advent-of-code-day-18-2019-the-real-challenge-aea3d4e96708
; to model the search space as a graph with each node being a tuple of the x,y 
; coordinate of the current position plus the set of keys that have been 
; acquired so far -> [x y key-set] 

(defn adjacent-part1
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


(deftest test-adjacent-part1
  (let [{:keys [maze]} (read-input ex1)]
    (testing "can move to spaces but not walls"
      (is (= #{[4 1 #{}] [6 1 #{}]} (adjacent-part1 maze [5 1 #{}]))))
    (testing "moving to a cell with a key puts the key in key set"
      (is (= #{[5 1 #{}] [7 1 #{:key/a}]} (adjacent-part1 maze [6 1 #{}]))))
    (testing "moving to a key which has already been obtained"
      (is (= #{[5 1 #{:key/a}] [7 1 #{:key/a}]} (adjacent-part1 maze [6 1 #{:key/a}]))))
    (testing "can move to cell with door if key is in key set"
      (is (= #{[5 1 #{}]} (adjacent-part1 maze [4 1 #{}])))
      (is (= #{[3 1 #{:key/a}] [5 1 #{:key/a}]} (adjacent-part1 maze [4 1 #{:key/a}]))))))



; https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode
; 1  function Dijkstra(Graph, source):
; 2
; 3      create vertex set Q
; 4
; 5      for each vertex v in Graph:             
; 6          dist[v] ← INFINITY                  
; 7          prev[v] ← UNDEFINED                 
; 8          add v to Q                      
;10      dist[source] ← 0                        
;11      
;12      while Q is not empty:
;13          u ← vertex in Q with min dist[u]    
;14                                              
;15          remove u from Q 
;16          
;17          for each neighbor v of u:           // only v that are still in Q
;18              alt ← dist[u] + length(u, v)
;19              if alt < dist[v]:               
;20                  dist[v] ← alt 
;21                  prev[v] ← u 
;22
;23      return dist[], prev[]

(defn has-all-keys?
  [pos all-keys]
  (= all-keys (last pos)))


(defn dijkstra-part1
  [{:keys [all-keys maze] [start-x start-y] :start}]
  (let [pq (PriorityQueue. 10000 (fn [x y] (compare (:dist x) (:dist y))))
        all-dist (HashMap.) 
        prev (HashMap.) 
        visited (HashSet.)
        min-end (volatile! nil)]
    (.add pq {:pos [start-x start-y #{}] :dist 0})
    (while (and (not= 0 (.size pq)) (nil? @min-end))
      (let [{u :pos dist :dist} (.poll pq)]
        (if (has-all-keys? u all-keys)
          (vreset! min-end {:dist dist :pos u})
          (do
            (doseq [v (adjacent-part1 maze u)]
              (when (not (.contains visited v))
                (let [alt (inc dist)
                      dist-v (get all-dist v Long/MAX_VALUE)]
                  (when (< alt dist-v)
                    (.remove pq {:pos v :dist dist-v})
                    (.add pq {:pos v :dist alt})
                    (.put all-dist v alt)
                    (.put prev v u)))))
            (.add visited u)))))
    (:dist @min-end)))


(deftest test-part-1
  (is (= 8 (dijkstra-part1 (read-input ex1))))
  (is (= 86 (dijkstra-part1 (read-input ex2))))
  (is (= 132 (dijkstra-part1 (read-input ex3))))
  #_(is (= 136 (dijkstra-part1 (read-input ex4))))
  (is (= 81 (dijkstra-part1 (read-input ex5)))))


#_(time (dijkstra-part1 (read-input))) ; ~ 165s, ~ 2:45min, 5406 steps


(comment
  (let [b1 (doto (java.util.BitSet. 26)
             (.set 25 true)
             (.set 24 true)
             (.set 23 true))
        b2 (doto (java.util.BitSet. 26)
             (.set 25 true)
             (.set 24 true)
             (.set 23 true))
        b3 (doto (java.util.BitSet. 26)
             (.set 25 true))]
    (prn (= b1 b2))
    (prn (= b2 b3))
    (prn (= b1 b3)))
  
  )


; part 2

(defn vec+
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])


(defn part2-maze
  [{:keys [maze start] :as input}]
  (let [from-start (partial vec+ start)
        new-walls (map from-start [up down left right [0 0]])
        new-starts (into [] 
                         (map from-start)
                         [[-1 -1] [1 -1] [-1 1] [1 1]])]
    (-> (reduce (fn [in [x y]]
                  (assoc-in in [:maze y x] :wall))
                input
                new-walls)
        (dissoc :start)
        (assoc :starts new-starts))))


(def ex6 "#######
#a.#Cd#
##...##
##.@.##
##...##
#cB#Ab#
#######")

(def ex7 "###############
#d.ABC.#.....a#
######...######
######.@.######
######...######
#b.....#.....c#
###############")

(def ex8 "#############
#DcBa.#.GhKl#
#.###...#I###
#e#d#.@.#j#k#
###C#...###J#
#fEbA.#.FgHi#
#############")

(def ex9 "#############
#g#f.D#..h#l#
#F###e#E###.#
#dCba...BcIJ#
#####.@.#####
#nK.L...G...#
#M###N#H###.#
#o#m..#i#jk.#
#############")


(defn render-input
  [{m :maze [w h] :size ss :starts}]
  (doseq [y (range h)]
    (doseq [x (range w)]
      (let [cell (get-in m [y x])]
        (print (cond (contains? ss [x y]) \@
                     (= cell :wall) \#
                     (= cell :space) \.
                     (= cell :start) \@
                     (door? cell) (string/upper-case (name cell))
                     (key? cell) (name cell)))))
    (println)))

#_(= ex6 (string/trim (with-out-str (render-input (read-input ex6)))))
#_(render-input (part2-maze (read-input ex6)))
#_(clojure.pprint/pprint (part2-maze (read-input ex6)))



; using hint from https://medium.com/@werner.altewischer/advent-of-code-day-18-2019-the-real-challenge-aea3d4e96708
; to model each node in the search space graph as a 9 tuple:
; [robot-a-pos robot-b-pos robot-c-pos robot-d-pos key-set]
; [key-set [x0 y0] [x1 y1] [x2 y2] [x3 y3]]

(def ex2b "#######
#a.#Cd#
##@#@##
#######
##@#@##
#cB#Ab#
#######")

#_(let [{:keys [maze starts]} (part2-maze (read-input ex6))]
    (clojure.pprint/pprint
      (adjacent-part2 maze (conj (vec starts) #{}))))
(defn adjacent-part2
  "Returns all possible adjacent nodes from the given node. Valid moves are
  moving any single robot one space. If a key is encountered it is added to the
  key set. A robot can only move onto a door if the key is already in the key
  set. Wrinkle: a robot should in some cases be able to put a key back down to
  be able to back track. This was not noticed in part 1 but the correct answer
  was still found. This function doesn't handle that case."
  [maze [r0 r1 r2 r3 ks :as coord]]
  (reduce (fn [adj [idx dir]]
            (let [[x y :as p] (nth coord idx)
                  [nx ny :as np] (vec+ p dir)
                  c (get-in maze [ny nx])]
              (cond ; it's a key, pick it up.
                    (key? c)
                    (conj adj (-> (assoc coord idx np)
                                  (update 4 conj c)))
                    ; it's a door and we have the key.
                    (and (door? c) (contains? ks (door->key c)))
                    (conj adj (assoc coord idx np))
                    ; it's a space
                    (#{:start :space} c)
                    (conj adj (assoc coord idx np))
                    ; off the maze, a wall, or a door we don't have the key for 
                    :else
                    adj)))
          #{}
          (for [idx (range 4)
                dir [up down left right]]
            [idx dir])))


(deftest test-adjacent-part2
  (let [{maze :maze [r0 r1 r2 r3] :starts} (part2-maze (read-input ex6))
        pos [r0 r1 r2 r3 #{}]]
    (is (= #{[[2 1] r1 r2 r3 #{}]}
           (adjacent-part2 maze pos)))
    (is (= #{[[1 1] r1 r2 r3 #{:key/a}]
             pos}
           (adjacent-part2 maze [[2 1] r1 r2 r3 #{}])))
    (is (= #{[[2 1] r1 r2 r3 #{:key/a}]
             [[1 1] r1 r2 [4 5] #{:key/a}]}
           (adjacent-part2 maze [[1 1] r1 r2 r3 #{:key/a}])))))


(defn dijkstra-part2
  [{:keys [all-keys maze] [r0 r1 r2 r3] :starts}]
  (let [pq (PriorityQueue. 10000 (fn [x y] (compare (:dist x) (:dist y))))
        all-dist (HashMap.) 
        prev (HashMap.) 
        visited (HashSet.)
        min-end (volatile! nil)]
    (.add pq {:pos [r0 r1 r2 r3 #{}] :dist 0})
    (while (not= 0 (.size pq))
      (let [{u :pos dist :dist} (.poll pq)]
        (when (and (has-all-keys? u all-keys)
                   (< dist (:dist @min-end Long/MAX_VALUE)))
          (vreset! min-end {:dist dist :pos u}))
        (doseq [v (adjacent-part2 maze u)]
          (when (not (.contains visited v))
            (let [alt (inc dist)
                  dist-v (get all-dist v Long/MAX_VALUE)]
              (when (< alt dist-v)
                (.remove pq {:pos v :dist dist-v})
                (.add pq {:pos v :dist alt})
                (.put all-dist v alt)
                (.put prev v u)))))
        (.add visited u)))
    (:dist @min-end)))


(deftest test-dijkstra-part2
  #_(is (= 8 (dijkstra-part2 (part2-maze (read-input ex6)))))
  #_(is (= 24 (dijkstra-part2 (part2-maze (read-input ex7)))))
  #_(is (= 32 (dijkstra-part2 (part2-maze (read-input ex8)))))
  #_(is (= 72 (dijkstra-part2 (part2-maze (read-input ex9))))))


#_(time (dijkstra-part2 (part2-maze (read-input))))
#_(.maxMemory (Runtime/getRuntime))
