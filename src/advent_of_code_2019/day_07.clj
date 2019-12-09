(ns advent-of-code-2019.day-07
  (:require [advent-of-code-2019.day-05 :as d5]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer :all]))


(defn read-input
  []
  (slurp (io/resource "day_07.txt")))


#_(permutations #{1})
#_(permutations #{1 2})
#_(permutations #{1 2 3})
#_(permutations #{0 1 2 3 4})
(defn permutations
  [syms]
  (cond (empty? syms) syms
        (= 1 (count syms)) [(seq syms)]
        :else (mapcat (fn [s]
                        (map (fn [sub-perm]
                               (cons s sub-perm)) 
                             (permutations (disj syms s))))
                      syms)))

(defn run-amps
  [program settings]
  (reduce (fn [last-out setting]
            (-> (d5/run-program program [setting last-out])
                last
                last))
          0
          settings))


(deftest test-run-amps
  (is (= 43210 (run-amps (d5/parse-input "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
                         [4 3 2 1 0])))
  (is (= 54321 (run-amps (d5/parse-input "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0" )
                         [0 1 2 3 4])))
  (is (= 65210 (run-amps (d5/parse-input "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0" )
                         [1 0 4 3 2]))))

#_(solve-part-1)
(defn solve-part-1
  ([] (solve-part-1 (d5/parse-input (read-input))))
  ([program]
   (transduce
     (map (partial run-amps program))
     max
     Long/MIN_VALUE
     (permutations #{0 1 2 3 4}))))


(deftest test-solve-part1
  (is (= 43210 (solve-part-1 (d5/parse-input "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))))
  (is (= 54321 (solve-part-1 (d5/parse-input "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0" ))))
  (is (= 65210 (solve-part-1 (d5/parse-input "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0" )))))
