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
  (is (= :op/output (get-opcode 104))))


(defn get-param
  [n modes program p]
  ((get modes n (param-modes \0)) program p))


(defn run-program
  ([program input]
   (run-program program 0 input []))
  ([program pos input output]
   (let [[inst a0 a1 a2] (drop pos program)
         op (get-opcode inst)
         param-modes (get-param-modes inst)]
     (cond (= op :op/halt) [program output] 
           (= op :op/add) (recur (assoc program a2 (+ (get-param 0 param-modes program a0)
                                                      (get-param 1 param-modes program a1)))
                                 (+ pos 4)
                                 input
                                 output) 
           (= op :op/mult) (recur (assoc program a2 (* (get-param 0 param-modes program a0)
                                                       (get-param 1 param-modes program a1)))
                                  (+ pos 4)
                                  input
                                  output)
           (= op :op/input) (recur (assoc program a0 (first input))
                                   (+ pos 2)
                                   (rest input)
                                   output)
           (= op :op/output) (recur program
                                    (+ pos 2)
                                    input
                                    (conj output (get-param 0 param-modes program a0)))))))




(deftest test-run-program
  (testing "output"
    (is (= [2019 104 2019 4 2018] (last (run-program [104 2019 4 0 4 1 4 2 104 2018 99] [])))))
  (testing "add two inputs and output"
    (is (= [7] (last (run-program [3 0 3 1 1 0 1 0 4 0 99] [3 4]))))
    (is (= [7] (last (run-program [3 0 3 1 1 0 1 0 4 0 99] [4 3]))))
    (is (= [27] (last (run-program [3 0 3 1 1 0 1 0 4 0 99] [14 13])))))
  (testing "mult two inputs and output"
    (is (= [12] (last (run-program [3 0 3 1 2 0 1 0 4 0 99] [3 4]))))
    (is (= [12] (last (run-program [3 0 3 1 2 0 1 0 4 0 99] [4 3]))))
    (is (= [182] (last (run-program [3 0 3 1 2 0 1 0 4 0 99] [14 13])))))
  (testing "add two immediate and output"
    (is (= [7] (last (run-program [1101 4 3 0 4 0 99] []))))
    (is (= [10] (last (run-program [1101 3 7 0 4 0 99] [])))))
  (testing "mult two immediate and output"
    (is (= [12] (last (run-program [1102 4 3 0 4 0 99] []))))
    (is (= [21] (last (run-program [1102 3 7 0 4 0 99] [])))))
  (testing "example from problem"
    (is (= [1002 4 3 4 99] (first (run-program [1002 4 3 4 33] [])))))
  (testing "double the input 3 times"
    (is (= [24] (last (run-program [3 0 102 2 0 0 102 2 0 0 102 2 0 0 4 0 99] [3]))))))


#_(solve-part-1)
(defn solve-part-1
  []
  (-> (run-program (parse-input (read-input)) [1])
      (last)
      (last)))
