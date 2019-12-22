(ns advent-of-code-2019.day-11
  (:require [advent-of-code-2019.intcode :as intcode]
            [clojure.spec.alpha :as spec]))



(spec/def :robot/pos (spec/tuple int? int?))
(spec/def :robot/heading #{:N :E :S :W})
(spec/def :hull/color #{:B :W})
(spec/def :robot/painted (spec/map-of :robot/pos :hull/color))
(spec/def :robot/computer map?)
(spec/def ::robot (spec/keys :req [:robot/pos
                                   :robot/heading
                                   :robot/painted
                                   :robot/computer]))


(defn initial-robot
  ([] (initial-robot (intcode/read-program "day_11.txt")))
  ([program]
   #:robot{:pos [0 0]
           :heading :N
           :painted {}
           :computer (intcode/run (intcode/new-computer program))}))


(def next-heading
  {:N {:left :W :right :E}
   :E {:left :N :right :S}
   :S {:left :E :right :W}
   :W {:left :S :right :N}})


(def turn-mapping
  {0 :left
   1 :right})


(def input-mapping
  {:B 0
   :W 1})


(def output-mapping
  {0 :B
   1 :W})


(def heading-vectors
  {:N [0 -1]
   :E [1 0]
   :W [-1 0]
   :S [0 1]})


(defn vec-add
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])


(defn move
  [robot heading]
  (-> robot
      (assoc :robot/heading heading)
      (update :robot/pos vec-add (get heading-vectors heading))))


(defn get-color
  [{:keys [robot/painted robot/pos]}]
  (get painted pos :B))


(defn paint
  [{:keys [robot/pos] :as robot} color]
  (assoc-in robot [:robot/painted pos] color))


(defn done?
  [robot]
  (not= :state/need-input (get-in robot [:robot/computer :state])))


(defn step-computer
  [computer color]
  (assert (= :state/need-input (:state computer)))
  (let [s0 (intcode/resume-with-input computer color)
        out0 (intcode/get-output s0)
        s1 (intcode/resume-from-output s0)
        out1 (intcode/get-output s1)]
    {:computer (intcode/resume-from-output s1) 
     :new-color (get output-mapping out0)
     :turn (get turn-mapping out1)}))


(defn next-robot*
  [{:keys [robot/heading] :as robot} {:keys [computer new-color turn]}]
  (-> robot
      (assoc :robot/computer computer)
      (paint new-color)
      (move (get-in next-heading [heading turn]))))


(defn next-robot
  [{:keys [robot/computer] :as robot}]
  (let [curr-color (get-color robot)
        result (step-computer computer (get input-mapping curr-color))]
    (next-robot* robot result)))


(defn run-robot
  [{:keys [robot/pos robot/computer robot/painted robot/heading] :as robot}]
  (if (done? robot)
    robot
    (recur (next-robot robot))))


#_(= (solve-part1) 1951)
(defn solve-part1
  []
  (-> (initial-robot)
      (run-robot)
      :robot/painted
      count))


(defn render
  [painted]
  (let [ps (keys painted)
        xs (map first ps)
        ys (map second ps)
        min-x (reduce min xs)
        max-x (reduce max xs)
        min-y (reduce min ys)
        max-y (reduce max ys)
        border 3
        render-chars {:B " " :W "#"}]
    (doseq [y (range (- min-y border) (+ max-y border))]
      (doseq [x (range (- min-x border) (+ max-x border))]
        (print (get render-chars (get painted [x y] :B))))
      (println))))


#_(solve-part2)
(defn solve-part2
  []
  (let [{:keys [robot/computer] :as robot} (initial-robot)
        first-result (step-computer computer (get input-mapping :W))
        {:keys [robot/painted]} (-> (next-robot* robot first-result)
                                    (run-robot))]
    (render painted)))

