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


(defn run-program
  ([program]
   (run-program program []))
  ([program input]
   (run-program program 0 input))
  ([program pos input]
   (let [[inst & args] (drop pos program)
         op (d5/get-opcode inst)
         get-p (partial d5/get-param (d5/get-param-modes inst) program args)]
     (cond (= op :op/halt) [:result/halt program]
           (contains? d5/binary-ops op) (let [p0 (get-p 0) 
                                              p1 (get-p 1)
                                              v ((get d5/binary-ops op) p0 p1)]
                                          (recur (assoc program (nth args 2) v)
                                                 (+ pos 4)
                                                 input))
           (contains? d5/jump-ops op) (let [t? (d5/long->bool (get-p 0))
                                            jump? (if (= op :op/jump-if-not) (not t?) t?)
                                            jump-pos (get-p 1)]
                                        (recur program 
                                               (if jump? jump-pos (+ pos 3))
                                               input))
           (= op :op/input) (if (empty? input)
                              [:result/need-input program pos]
                              (recur (assoc program (nth args 0) (first input))
                                     (+ pos 2)
                                     (rest input)))
           (= op :op/output) [:result/output program (+ pos 2) input (get-p 0)]
           :else (throw (ex-info "invalid op" {:program program
                                               :pos pos
                                               :op op}))))))


(defn resume-with-input
  [[rtype program pos :as result] input]
  (assert (= rtype :result/need-input))
  (run-program program pos [input]))


(defn resume-after-output
  [[rtype program pos input output :as result]]
  (assert (= rtype :result/output))
  (run-program program pos input))


(defn get-output
  [[rtype program pos input output :as result]]
  (assert (= rtype :result/output))
  output)


(defn result-type
  [[rtype :as result]]
  rtype)


(deftest test-run-program
  (let [prg [3 0 ; input -> 0
             3 1 ; input -> 1
             1 0 1 0 ; add p[0] = p[0] + p[1] 
             4 0 ; output <- 0
             3 1 ; input -> 1
             101 1 1 0 ; p[0] = 1 + p[1]
             4 0 ;output <- 0
             99]
        r0 (run-program prg)
        r1 (resume-with-input r0 33)
        r2 (resume-with-input r1 44)
        out0 (get-output r2)
        r3 (resume-after-output r2)
        r4 (resume-with-input r3 100)
        out1 (get-output r4)
        r5 (resume-after-output r4)]
    (is (= out0 77))
    (is (= out1 101))
    (is (not= :result/halt (result-type r0)))
    (is (not= :result/halt (result-type r1)))
    (is (not= :result/halt (result-type r2)))
    (is (not= :result/halt (result-type r3)))
    (is (not= :result/halt (result-type r4)))
    (is (= :result/halt (result-type r5)))))


(defn init-amps
  [program settings]
  (mapv #(run-program program [%]) settings))


(deftest test-init-amps
  (testing "all amps waiting for input"
    (let [amps (init-amps (d5/parse-input (read-input)) [9 8 7 6 5])]
      (is (every? #(= :result/need-input (result-type %)) amps))
      (is (= 5 (count amps))))))


(defn all-halted?
  [amps]
  (every? #(= :result/halt (result-type %)) amps))


(defn run-round
  ([amps input] (run-round amps [] input))
  ([[amps input :as last-round]] (run-round amps input))
  ([[a & in-amps] out-amps input]
   (if (some? a)
     (let [r0 (resume-with-input a input)
           r1 (resume-after-output r0)]
       (recur in-amps
              (conj out-amps r1)
              (get-output r0)))
     [out-amps input])))


(defn compile-program
  [src]
  (let [vars (into #{} (filter keyword?) src)
        var-pos (zipmap vars (range (count src)
                                    (+ (count src) (count vars))))]
    (into (mapv #(if-let [p (get var-pos %)]
                   p
                   %)
                src)
          (repeat (count vars) 0))))


(deftest test-run-round
  (let [prg (compile-program [3 :x                        ; input -> :x
                              101 1 :run-count :run-count ; :run-count = :run-count + 1 
                              4 :run-count                ; output <- :run-count
                              1007 :run-count 3 :done-p   ; :done-p = :run-count < 3 
                              1005 :done-p 0              ; :jump-if :done-p 0 
                              99                          ; :halt
                              ]) 
        amps0 (repeatedly 5 #(run-program prg))
        round-result1 (run-round amps0 0)
        round-result2 (run-round round-result1)
        [amps out] (run-round round-result2)]
    (is (not (all-halted? amps0)))
    (is (not (all-halted? (first round-result1))))
    (is (not (all-halted? (first round-result2))))
    (is (all-halted? amps))
    (is (= 3 out))))


(defn run-amps2
  [amps inital-input]
  (loop [[amps out :as result] (run-round amps inital-input)]
    (if (all-halted? amps)
      out
      (recur (run-round amps out)))))


(deftest test-run-amps2
  (is (= 139629729 (run-amps2 (init-amps (d5/parse-input "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5") 
                                         [9 8 7 6 5])
                              0)))
  (is (= 18216 (run-amps2 (init-amps (d5/parse-input "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10") 
                                     [9 7 8 65 6])
                          0))))

#_(prn (solve-part-2))
(defn solve-part-2
  ([] (solve-part-2 (d5/parse-input (read-input))))
  ([program]
   (transduce
     (map (fn [settings]
            (run-amps2 (init-amps program settings) 0)))
     max
     Long/MIN_VALUE
     (permutations #{5 6 7 8 9}))))


(deftest test-solve-part-2
  (is (= 139629729 (solve-part-2 (d5/parse-input "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"))))
  (is (= 18216 (solve-part-2 (d5/parse-input "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")))))
