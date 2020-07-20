(ns advent-of-code-2019.day-17
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [advent-of-code-2019.intcode :as intcode]))


#_(println (apply str (get-photo)))
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

; part 2

; strategy, infer a program from the a set of points that the scafolding 
; occupies. Figured out ways to shorten it and split it up into sub programs
; so that it fits into memory
;
; longest common subssequence?

(defn vec+
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

(def heading->unit-vec
  {:heading/north [0 -1]
   :heading/south [0 1]
   :heading/east [1 0]
   :heading/west [-1 0]})

(def heading->turn->heading
  {:heading/north {:turn/right :heading/east
                   :turn/left :heading/west}
   :heading/south {:turn/right :heading/west
                   :turn/left :heading/east}
   :heading/east {:turn/left :heading/north
                  :turn/right :heading/south}
   :heading/west {:turn/left :heading/south
                  :turn/right :heading/north}})

(defn next-heading
  [heading turn]
  (get-in heading->turn->heading [heading turn]))

(defn move-one
  [pos heading]
  (vec+ pos (heading->unit-vec heading)))


(def init-pos [[26 16] :heading/north])
(defn find-corner-from
  [ps [pos heading]]
  (let [next-pos (move-one pos heading)]
    (if (contains? ps next-pos)
      ; recursive case, move pos ahead by 1
      (recur ps [next-pos heading])
      ; base cases
      (let [right-heading (next-heading heading :turn/right)
            right (move-one pos right-heading)
            left-heading (next-heading heading :turn/left)
            left (move-one pos left-heading)]
        (cond
          ; special case, T shaped junction. Assuming doesn't happen. 
          (and (contains? ps right) (contains? ps left)) (throw (IllegalStateException. "Ran into T!"))
          ; found corner to the right
          (contains? ps right) [pos :pos/corner :turn/right right-heading]
          ; found corner to the left
          (contains? ps left) [pos :pos/corner :turn/left left-heading]
          ; found the end of the scaffolding
          :else [pos :pos/end])))))


(defn dist
  [[x0 y0] [x1 y1]]
  (Math/abs
    (cond (= x0 x1) (- y1 y0)
          (= y0 y1) (- x1 x0)
          :else (throw (IllegalStateException. "robot trying to move diagnoally!")))))


(defn find-path
  [ps [pos heading] program]
  (let [[next-pos pos-type turn next-heading] (find-corner-from ps [pos heading])]
    (if (= pos-type :pos/end)
      (rest (conj program (dist pos next-pos)))
      (recur ps [next-pos next-heading] (into program [(dist pos next-pos) turn])))))

(def turn->ascii
  {:turn/left \L
   :turn/right \R})

(defn post-process
  [program]
  (into [] 
        (comp (interpose \,)
              (mapcat (fn [x]
                        (cond (contains? turn->ascii x) [(turn->ascii x)]
                              (number? x) (seq (str x)) 
                              :else [x]))))
        program))

#_(clojure.pprint/pprint (sub-str-table "aaabbbaaaaaabbbbbb"))
(defn sub-str-table
  [xs]
  (let [len (count xs)
        table (atom {})]
    (doseq [i (range 1 (inc len))]
      (doseq [j (range i (inc len))]
        (let [prev-len (get @table [(dec i) (dec j)] 0)]
          (if (and (= (nth xs (dec i))
                      (nth xs (dec j)))
                   (< prev-len (- j i)))
            (swap! table assoc [i j] (inc prev-len))))))
    @table))

#_(clojure.pprint/pprint (sort-by (comp - val) (sub-str-table needed-prg)))
(defn get-sub-strs
  [xs min-len]
  (into #{}
        (comp (filter #(<= min-len (val %)))
              (map (fn [[[i j] len]]
                     (take len (drop (- len i) xs)))))
        (sub-str-table xs)))

; [[26 68] 14]
(def needed-prg
  [:turn/left
   10
   :turn/right
   12
   :turn/right
   12
   :turn/right
   6
   :turn/right
   10
   :turn/left
   10
   :turn/left
   10
   :turn/right
   12
   :turn/right
   12
   :turn/right
   10
   :turn/left
   10
   :turn/left
   12
   :turn/right
   6
   :turn/right
   6
   :turn/right
   10
   :turn/left
   10
   :turn/right
   10
   :turn/left
   10
   :turn/left
   12
   :turn/right
   6
   :turn/right
   6
   :turn/right
   10
   :turn/left
   10
   :turn/right
   10
   :turn/left
   10
   :turn/left
   12
   :turn/right
   6
   :turn/left
   10
   :turn/right
   12
   :turn/right
   12
   :turn/right
   10
   :turn/left
   10
   :turn/left
   12
   :turn/right
   6])

(test-prg {:main [:A]
           :A needed-prg
           :B []
           :C []
           })

(defn test-prg
  [{:keys [main A B C] :as prg}]
  (assert (every? #{:A :B :C} main))
  (assert (every? #(or (number? %) (contains? #{:turn/right :turn/left} %)) A))
  (assert (every? #(or (number? %) (contains? #{:turn/right :turn/left} %)) B))
  (assert (every? #(or (number? %) (contains? #{:turn/right :turn/left} %)) C))
  (= needed-prg (mapcat prg main)))



(comment
  (def ps (get-scaffolding-points (get-photo)))
  (prn ps)
  26 16

  (find-corner-from ps [[26 16] :heading/north])
  (with-open [wr (io/writer "day_17_prg.edn")]
    (clojure.pprint/pprint (vec (find-path ps [[26 16] :heading/north] [])) wr))

  (count (post-process (find-path ps [[26 16] :heading/north] [])))

  (get-sub-strs "aaabbbaaaaaabbbbbb" 2)

  (->> (sub-str-table "aaabbbaaaaaabbbbbb")
       (filter #(< 3 (val %)))
       (map (fn [[[i j] len]]
              [(get-sub "aaabbbaaaaaabbbbbb" i len)
               (get-sub "aaabbbaaaaaabbbbbb" j len)])))

  )
