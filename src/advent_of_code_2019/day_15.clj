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


(defn update-qs
  [qs moveable state pos]
  (-> qs
      (update :q pop)
      (update :q into (map #(next-state state %) moveable))
      (update :visited conj pos)))


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
          (update-qs qs moveable state pos))))
    ; empty queue before o2 tank found :[
    {:done? true :result :empty-queue}))


(defn iterate-until-done
  [step-fn init]
  (->> (iterate step-fn init)
       (drop-while (comp not :done?))
       (first)))


#_(find-min-moves) ; 262, [12 -14]
(defn find-min-moves
  []
  (-> (iterate-until-done step
                          {:q (into clojure.lang.PersistentQueue/EMPTY [init-state])
                           :visited #{}})))


(defn map-step
  [{:keys [q visited] :as qs}]
  (if-let [{:keys [computer pos] :as state} (peek q)]
    (if (contains? visited pos)
      (update qs :q pop)
      (let [{moveable move [dest-dir] dest} (survey computer)
            dirs (if (some? dest-dir) (conj moveable dest-dir) moveable)]
        (update-qs qs dirs state pos)))
    {:done? true :result visited}))


#_(map-space)
(defn map-space
  []
  (-> (iterate-until-done map-step {:q (into clojure.lang.PersistentQueue/EMPTY [init-state])
                                    :visited #{}})
      :result))


(defn find-bounds
  [visited]
  (reduce (fn [[min-x min-y max-x max-y] [x y]]
            [(min min-x x)
             (min min-y y)
             (max max-x x)
             (max max-y y)])
          [Long/MAX_VALUE Long/MAX_VALUE Long/MIN_VALUE Long/MIN_VALUE]
          visited))


#_(spit "day15_maze.txt" (with-out-str (plot-visited (map-space) [0 0] [12 -14])))
(defn plot-visited
  [visited start end]
  (let [[min-x min-y max-x max-y] (find-bounds visited)]
    (doseq [y (range min-y (inc max-y))]
      (doseq [x (range min-x (inc max-x))]
        (let [p [x y]]
          (print (cond (= p start) "S"
                       (= p end) "E"
                       (contains? visited p) " "
                       :else "*"))))
      (println))))


(defn spread-from
  [visited p]
  (sequence (comp (map dir->unit-vec)
                  (map (partial vec+ p))
                  (filter visited))
            [north south east west]))


(defn spread-step
  [{:keys [air visited steps] :as state}]
  (if (= air visited)
    (assoc state :done? true) 
    (-> state
        (update :steps inc)
        (update :air #(into air (mapcat (partial spread-from visited)) %)))))


#_(steps-to-spread) ; 314
(defn steps-to-spread
  []
  (-> (iterate-until-done spread-step {:done? false
                                       :visited (map-space) 
                                       :steps 0
                                       :air #{[12 -14]}})
      :steps))

