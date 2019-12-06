(ns advent-of-code-2019.day-05
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
  (slurp (io/resource "day_05.txt")))


(def param-modes
  {\0 (fn [program d] (get program d))
   \1 (fn [program d] d)})


(defn get-param-modes 
  [inst]
  (mapv param-modes (drop 2 (reverse (str inst)))))


(def opcodes
  {"01" :op/add 
   "02" :op/mult
   "03" :op/input 
   "04" :op/output
   "05" :op/jump-if
   "06" :op/jump-if-not
   "07" :op/less-than
   "08" :op/equals
   "99" :op/halt})


(defn get-opcode
  [inst]
  (if (some? inst)
    (let [s (str inst)]
      (get opcodes
           (if (< (count s) 2)
             (str "0" s)
             (subs s (- (count s) 2)))))
    :op/halt))


(deftest test-get-opcode
  (is (= :op/halt (get-opcode nil)))
  (is (= :op/halt (get-opcode 99)))
  (is (= :op/add (get-opcode 1)))
  (is (= :op/add (get-opcode 1001)))
  (is (= :op/add (get-opcode 1101)))
  (is (= :op/mult (get-opcode 2)))
  (is (= :op/mult (get-opcode 1002)))
  (is (= :op/mult (get-opcode 1102)))
  (is (= :op/input (get-opcode 3)))
  (is (= :op/output (get-opcode 4)))
  (is (= :op/output (get-opcode 104)))
  (is (= :op/jump-if (get-opcode 1105)))
  (is (= :op/jump-if (get-opcode 105)))
  (is (= :op/jump-if (get-opcode 5)))
  (is (= :op/jump-if-not (get-opcode 1106)))
  (is (= :op/jump-if-not (get-opcode 106)))
  (is (= :op/jump-if-not (get-opcode 6)))
  (is (= :op/less-than (get-opcode 1107)))
  (is (= :op/less-than (get-opcode 107)))
  (is (= :op/less-than (get-opcode 7)))
  (is (= :op/equals (get-opcode 1108)))
  (is (= :op/equals (get-opcode 108)))
  (is (= :op/equals (get-opcode 8))))


(defn get-param
  [modes program args n]
  ((get modes n (param-modes \0)) program (nth args n)))


(defn bool->long
  [b]
  (if b 1 0))

(defn long->bool
  [i]
  (if (zero? i) false true))


(def jump-ops #{:op/jump-if :op/jump-if-not})
(def binary-ops {:op/add +
                 :op/mult *
                 :op/less-than (comp bool->long <)
                 :op/equals (comp bool->long =)})


(defn run-program
  ([program input]
   (run-program program 0 input []))
  ([program pos input output]
   (let [[inst & args] (drop pos program)
         op (get-opcode inst)
         get-p (partial get-param (get-param-modes inst) program args)]
     (cond (= op :op/halt) [program output] 
           (contains? binary-ops op) (let [p0 (get-p 0) 
                                           p1 (get-p 1)
                                           v ((get binary-ops op) p0 p1)]
                                       (recur (assoc program (nth args 2) v)
                                              (+ pos 4)
                                              input
                                              output))
           (contains? jump-ops op) (let [t? (long->bool (get-p 0))
                                         jump? (if (= op :op/jump-if-not) (not t?) t?)
                                         jump-pos (get-p 1)]
                                     (recur program 
                                            (if jump? jump-pos (+ pos 3))
                                            input
                                            output))
           (= op :op/input) (recur (assoc program (nth args 0) (first input))
                                   (+ pos 2)
                                   (rest input)
                                   output)
           (= op :op/output) (recur program
                                    (+ pos 2)
                                    input
                                    (conj output (get-p 0)))))))


(deftest test-run-program
  (testing "output"
    (is (= [2019 104 2019 4 2018] (last (run-program [104 2019 4 0 4 1 4 2 104 2018 99] [])))))
  (testing "add two inputs and output"
    (let [prg [3 0 3 1 1 0 1 0 4 0 99]]
      (is (= [7] (last (run-program prg [3 4]))))
      (is (= [7] (last (run-program prg [4 3]))))
      (is (= [27] (last (run-program prg [14 13]))))))
  (testing "mult two inputs and output"
    (let [prg [3 0 3 1 2 0 1 0 4 0 99]]
      (is (= [12] (last (run-program prg [3 4]))))
      (is (= [12] (last (run-program prg [4 3]))))
      (is (= [182] (last (run-program prg [14 13]))))))
  (testing "add two immediate and output"
    (is (= [7] (last (run-program [1101 4 3 0 4 0 99] []))))
    (is (= [10] (last (run-program [1101 3 7 0 4 0 99] [])))))
  (testing "mult two immediate and output"
    (is (= [12] (last (run-program [1102 4 3 0 4 0 99] []))))
    (is (= [21] (last (run-program [1102 3 7 0 4 0 99] [])))))
  (testing "double the input 3 times"
    (is (= [24] (last (run-program [3 0 102 2 0 0 102 2 0 0 102 2 0 0 4 0 99] [3])))))
  (testing "problem examples"
    (testing "simple mult"
      (is (= [1002 4 3 4 99] (first (run-program [1002 4 3 4 33] [])))))
    (testing "position mode, equal to 8?"
      (let [prg [3 9 8 9 10 9 4 9 99 -1 8]]
        (is (= [1] (last (run-program prg [8]))))
        (is (= [0] (last (run-program prg [9])))))) 
    (testing "position mode, less than 8?"
      (let [prg [3 9 7 9 10 9 4 9 99 -1 8]]
        (is (= [1] (last (run-program prg [7]))))
        (is (= [0] (last (run-program prg [8]))))
        (is (= [0] (last (run-program prg [9]))))))
    (testing "immediate mode, equal to 8?"
      (let [prg [3 3 1108 -1 8 3 4 3 99]]
        (is (= [1] (last (run-program prg [8]))))
        (is (= [0] (last (run-program prg [9]))))))
    (testing "immediate mode, less than 8?"
      (let [prg [3 3 1107 -1 8 3 4 3 99]]
        (is (= [1] (last (run-program prg [7]))))
        (is (= [0] (last (run-program prg [8]))))
        (is (= [0] (last (run-program prg [9]))))))
    (testing "<, =, or > 8?"
      (let [prg [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31 1106 0 36
                 98 0 0 1002 21 125 20 4 20 1105 1 46 104 999 1105 1 46 1101
                 1000 1 20 4 20 1105 1 46 98 99]]
        (is (= [999] (last (run-program prg [-100]))))
        (is (= [999] (last (run-program prg [7]))))
        (is (= [1000] (last (run-program prg [8]))))
        (is (= [1001] (last (run-program prg [9]))))
        (is (= [1001](last (run-program prg [100]))))))))


#_(prn (solve-part-1))
(defn solve-part-1
  []
  (-> (run-program (parse-input (read-input)) [1])
      last))


#_(prn (solve-part-2))
(defn solve-part-2
  []
  (-> (run-program (parse-input (read-input)) [5])
      last))
