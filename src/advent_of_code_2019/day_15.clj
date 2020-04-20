(ns advent-of-code-2019.day-15
  (:require [advent-of-code-2019.intcode :as intcode]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))


(def north 1)
(def south 2)
(def west 3)
(def east 4)

(def wall 0)
(def move 1)
(def dest 2)


(def init-state
  {:computer (-> (intcode/read-program "day_15.txt")
                 (intcode/new-computer)
                 (intcode/run))
   :pos [0 0] 
   :depth 0})


(defn vec+
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])


(def dir->unit-vec
  {north [0 1]
   south [0 -1]
   east [1 0]
   west [-1 0]})


(defn next-pos
  [pos dir]
  (vec+ pos (dir->unit-vec dir)))


(defn survey
  [computer]
  (let [status #(intcode/get-output (intcode/resume-with-input computer %))]
    (reduce (fn [r dir]
              (update r (status dir) conj dir))
            {}
            [north south east west])))


(defn next-state
  [{:keys [computer pos depth]} dir]
  (let [c0 (intcode/resume-with-input computer dir)
        status (intcode/get-output c0)]
    (assert (not= status wall) (str "got status: " status))
    {:computer (intcode/resume-from-output c0)
     :pos (next-pos pos dir)
     :depth (inc depth)}))


(defn step
  [{:keys [q visited] :as qs}]
  (if-let [{:keys [computer pos] :as state} (peek q)]
    (if (contains? visited pos)
      ; been here before!
      (update qs :q pop)
      ; see where we can move
      (let [{moveable move [dest-dir] dest} (survey computer)]
        (if (some? dest-dir)
          ; found the o2 tank!
          {:done? true :result (next-state state dest-dir)}
          ; queue places we can move
          (-> qs
              (update :q pop)
              (update :q into (map #(next-state state %) moveable))
              (update :visited conj pos)))))
    ; empty queue before o2 tank found :[
    {:done? true :result :empty-queue}))


#_(find-min-moves) ; 262
(defn find-min-moves
  []
  (first
    (drop-while (comp not :done?)
                (iterate step
                         {:q (into clojure.lang.PersistentQueue/EMPTY [init-state])
                          :visited #{}}))))



