(ns advent-of-code-2019.day-02
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer :all]))



(defn parse-input
  [input]
  (into []
        (comp (map string/trim)
              (map #(Long/valueOf %)))
        (string/split input #"\s?,\s?")))


(defn read-input
  []
  (parse-input (slurp (io/resource "day_02_part1.txt"))))


(def op->fn
  {1 +
   2 *})


(defn run-program*
  ([program noun verb] (run-program* (-> program
                                        (assoc 1 noun)
                                        (assoc 2 verb))
                                    0))
  ([program pos]
   (let [[op idx0 idx1 dest] (take 4 (drop pos program))]
     (if (= op 99)
       program
       (recur (assoc program dest ((op->fn op)
                                   (get program idx0)
                                   (get program idx1)))
              (+ pos 4))))))


(defn run-program
  [program noun verb]
  (first (run-program* program noun verb)))


(deftest test-run-program*
  (is (= [2 0 0 0 99] (run-program* [1 0 0 0 99] 0)))
  (is (= [2 3 0 6 99] (run-program* [2 3 0 3 99] 0)))
  (is (= [2 4 4 5 99 9801] (run-program* [2 4 4 5 99 0] 0)))
  (is (= [30 1 1 4 2 5 6 0 99] (run-program* [1 1 1 4 99 5 6 0 99] 0))))


#_(prn (solve-part-1))
(defn solve-part-1
  []
  (run-program (read-input) 12 2))


(defn finalize
  [[noun verb]]
  (+ (* 100 noun) verb))


#_(time (brute-force-part2 19690720)) ; ~1.5s
(defn brute-force-part2
  [goal]
  (->> (let [program (read-input)]
         (for [noun (range 100)
               verb (range 100)]
           [noun verb (run-program program noun verb)]))
       (drop-while (fn [[noun verb result]]
                     (not= result goal)))
       (first)
       (finalize)))


(comment
  ; determine if this function has a predictable shape
  ; export data
  (with-open [wr (io/writer "coords.txt")]
    (let [p (read-input)]
      (doseq [c (for [noun (range 100)
                      verb (range 100)]
                  [noun verb (run-program p noun verb)])]
        (.write wr (str (string/join " " c) "\n")))))

  ; plot with gnuplot
  ; $ gnuplot
  ; > splot 'coords.txt'
  ; See day2_plot.png
  ;
  ; It's a plane!
  ; The solution noun and verb are in the this data set so
  ; no need for a something better.

  )
