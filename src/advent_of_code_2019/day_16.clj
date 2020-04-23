(ns advent-of-code-2019.day-16
  (:require [clojure.test :refer :all]))


; wat (Math/abs 12123423222222)
(defn just-ones
  [x]
  (mod (if (neg? x) (- x) x) 10))


(defn mult-step
  [rep-seq input-seq]
  (->> (map * rep-seq input-seq)
       (reduce +)
       (just-ones)))


(defn get-rep-seq
  [n]
  (rest (cycle (concat (repeat n 0)
                       (repeat n 1)
                       (repeat n 0)
                       (repeat n -1)))))


#_(phase [1 2 3 4 5 6 7 8])
(defn phase
  ([input] (phase input 1))
  ([input n]
   (if (> n (count input))
     []
     (cons (mult-step (get-rep-seq n) input)
           (phase input (inc n))))))


(defn phase-times
  [input n]
  (last (take (inc n) (iterate phase input))))


(defn parse-input
  [s]
  (map #(Long/valueOf (str %)) s))


(deftest test-phase-times
  (testing "first example"
    (let [input [1 2 3 4 5 6 7 8]]
      (is (= [4 8 2 2 6 1 5 8] (phase-times input 1)))
      (is (= [3 4 0 4 0 4 3 8] (phase-times input 2)))
      (is (= [0 3 4 1 5 5 1 8] (phase-times input 3)))
      (is (= [0 1 0 2 9 4 9 8] (phase-times input 4)))))
  (testing "other examples"
    (let [ph #(take 8 (phase-times (parse-input %) 100))]
      (is (= [2 4 1 7 6 1 7 6] (ph "80871224585914546619083218645595")))
      (is (= [7 3 7 4 5 4 1 8] (ph "19617804207202209144916044189917")))
      (is (= [5 2 4 3 2 1 3 3] (ph "69317163492948606335995924319873"))))))


(def input "59793513516782374825915243993822865203688298721919339628274587775705006728427921751430533510981343323758576985437451867752936052153192753660463974146842169169504066730474876587016668826124639010922391218906707376662919204980583671961374243713362170277231101686574078221791965458164785925384486127508173239563372833776841606271237694768938831709136453354321708319835083666223956618272981294631469954624760620412170069396383335680428214399523030064601263676270903213996956414287336234682903859823675958155009987384202594409175930384736760416642456784909043049471828143167853096088824339425988907292558707480725410676823614387254696304038713756368483311")


#_(solve-part-1)
(defn solve-part-1
  []
  (time (take 8 (phase-times (parse-input input) 10)))




  )


