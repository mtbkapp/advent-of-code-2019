(ns advent-of-code-2019.intcode
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer :all]))


(defn wrap-int-bool
  [f]
  (fn [x y]
    (if (f x y) 1 0)))

(def opcodes
  {"01" {:op + :param-count 3 :op-type :binary} 
   "02" {:op * :param-count 3 :op-type :binary}
   "03" {:op :op/input :param-count 1 :op-type :io} 
   "04" {:op :op/output :param-count 1 :op-type :io}
   "05" {:op :op/jump-if :param-count 2 :op-type :jump}
   "06" {:op :op/jump-if-not :param-count 2 :op-type :jump}
   "07" {:op (wrap-int-bool <) :param-count 3 :op-type :binary}
   "08" {:op (wrap-int-bool =) :param-count 3 :op-type :binary}
   "09" {:op :op/rel-set :param-count 1 :op-type :relative}
   "99" {:op :op/halt :param-count 0 :op-type :halt}})


(def param-modes
  {\0 :mode/position
   \1 :mode/immediate
   \2 :mode/relative})


(defn parse-op
  [program instr-ptr]
  (let [[opcode a0 a1 a2] (drop instr-ptr program)
        [op0 op1 mode0 mode1] (reverse (str opcode))
        {:keys [param-count] :as op} (get opcodes (str (or op1 "0") op0))]
    (assoc op
           :param-modes (map #(get param-modes % :mode/position) [mode0 mode1])
           :args (take param-count [a0 a1 a2]))))


(deftest test-parse-op
  (testing "binary ops"
    (doseq [[c m0 m1] (for [c [1 2 7 8] m0 [0 1 2] m1 [0 1 2]] [c m0 m1])]
      (let [opcode (Long/valueOf (str m0 m1 "0" c))
            a0 7
            a1 8
            a2 9
            prg [-1 -1 opcode a0 a1 a2 10 11]
            {ps :param-modes opc :op :keys [param-count op-type args]} (parse-op prg 2)
            expected-modes (map (comp param-modes first str) [m1 m0])]
        (is (= (-> (get opcodes (str "0" c)) :op) opc))
        (is (= :binary op-type))
        (is (= args [a0 a1 a2]))
        (is (= 3 param-count))
        (is (= expected-modes ps)))))
  (testing "io ops"
    (let [{:keys [op op-type args]} (parse-op [3 9 99] 0)]
      (is (= [9] args))
      (is (= :io op-type))
      (is (= :op/input op)))
    (let [{:keys [op op-type args]} (parse-op [4 8 99] 0)]
      (is (= [8] args))
      (is (= :io op-type))
      (is (= :op/output op))))
  (testing "jump ops"
    (doseq [[code mode] (for [c [5 6] m [0 1 2]] [c m])]
      (let [opcode (Long/valueOf (str mode "0" code))
            a0 877
            a1 778
            prg [-1 opcode a0 a1 10 11 12]
            {ps :param-modes opc :op :keys [op-type param-count args]} (parse-op prg 1)
            expected-mode (get param-modes (first (str mode)))]
        (is (= (-> (get opcodes (str "0" code)) :op) opc))
        (is (= 2 param-count))
        (is (= [a0 a1] args))
        (is (= :jump op-type))
        (is (= expected-mode (first ps))))))
  (testing "rel set"
    (doseq [mode [0 1 2]]
      (let [opcode (Long/valueOf (str mode "09"))
            a0 777 
            prg [opcode a0 99]
            {opc :op ps :param-modes :keys [args param-count] :as op} (parse-op prg 0)
            expected-mode (get param-modes (first (str mode)))]
        (is (= :op/rel-set opc))
        (is (= 1 param-count))
        (is (= expected-mode (first ps)))
        (is (= [a0] args)))))
  (testing "halt"
    (let [{:keys [op]} (parse-op [99] 0)]
      (is (= :op/halt op)))))


(defn get-param
  [program rel-ptr arg mode]
  (case mode
    :mode/position (get program arg)
    :mode/relative (get program (+ rel-ptr arg))
    :mode/immediate arg))


(defn read-params
  [program rel-ptr {opc :op :keys [op-type args param-modes param-count] :as op}]
  (cond 
    (= :binary op-type) (let [[m0 m1] param-modes
                              [a0 a1 dest] args]
                          [(get-param program rel-ptr a0 m0)
                           (get-param program rel-ptr a1 m1)
                           dest])
    (= :jump op-type) (let [[m0 m1] param-modes
                            [a0 a1] args]
                        [(get-param program rel-ptr a0 m0)
                         (get-param program rel-ptr a1 m1)])
    (= :op/output opc) [(get-param program
                                   rel-ptr
                                   (first args)
                                   (first param-modes))]
    (= :op/input opc) [(first args)]
    (= :op/rel-set opc) [(get-param program
                                    rel-ptr
                                    (first args)
                                    (first param-modes))]
    :else []))


(deftest test-read-params
  (testing "binary ops"
    (let [prg [1 5 6 0 99 98 97]]
      (is (= [98 97 0]
             (read-params prg 1 (parse-op prg 0)))))
    (let [prg [101 5 6 0 99 98 97]]
      (is (= [5 97 0]
             (read-params prg 1 (parse-op prg 0)))))
    (let [prg [1001 5 6 0 99 98 97]]
      (is (= [98 6 0]
             (read-params prg 1 (parse-op prg 0)))))
    (let [prg [1101 5 6 0 99 98 97]]
      (is (= [5 6 0]
             (read-params prg 1 (parse-op prg 0)))))
    (let [prg [201 5 6 0 99 98 97]]
      (is (= [97 97 0]
             (read-params prg 1 (parse-op prg 0)))))
    (let [prg [2001 5 3 0 99 98 97]]
      (is (= [98 99 0]
             (read-params prg 1 (parse-op prg 0)))))
    (let [prg [2201 5 3 0 99 98 97]]
      (is (= [97 99 0]
             (read-params prg 1 (parse-op prg 0))))))
  (testing "jump ops"
    (let [prg [5 0 3 99]]
      (is (= [5 99]
             (read-params prg 1 (parse-op prg 0)))))
    (let [prg [1105 0 876 99]]
      (is (= [0 876]
             (read-params prg 1 (parse-op prg 0)))))
    (let [prg [2205 -1 2 99]]
      (is (= [2205 99]
             (read-params prg 1 (parse-op prg 0))))))
  (testing "output"
    (let [prg [4 2 99]]
      (is (= [99]
             (read-params prg 1 (parse-op prg 0)))))
    (let [prg [104 2 99]]
      (is (= [2]
             (read-params prg 1 (parse-op prg 0)))))
    (let [prg [204 -1 99]]
      (is (= [204]
             (read-params prg 1 (parse-op prg 0))))))
  (testing "input"
    (let [prg [3 2 99]]
      (is (= [2]
             (read-params prg 1 (parse-op prg 0))))))
  (testing "rel set"
    (let [prg [9 2 99]]
      (is (= [99]
             (read-params prg 1 (parse-op prg 0)))))
    (let [prg [109 2 99]]
      (is (= [2]
             (read-params prg 1 (parse-op prg 0)))))
    (let [prg [209 -1 99]]
      (is (= [209]
             (read-params prg 1 (parse-op prg 0)))))))


(defn expand-memory
  [computer max-i]
  (update computer
          :program
          (fn [prg]
            (if (< max-i (count prg))
              prg
              (into prg (repeat (- (inc max-i) (count prg)) 0))))))


(deftest test-expand-memory
  (let [c {:program [1 1 1]}]
    (is (= c (expand-memory c 0))) 
    (is (= c (expand-memory c 2)))
    (is (= [1 1 1 0] (:program (expand-memory c 3))))
    (is (= [1 1 1 0 0 0 0 0 0 0 0] (:program (expand-memory c 10))))))


(defn write
  [computer dest v]
  (-> (expand-memory computer dest)
      (update :program 
              assoc
              dest
              v)))


(deftest test-write
  (is (= [2 1 1] (:program (write {:program [1 1 1]} 0 2))))
  (is (= [1 1 2] (:program (write {:program [1 1 1]} 2 2))))
  (is (= [1 1 1 0 0 0 0 0 0 0 100] (:program (write {:program [1 1 1]} 10 100)))))


(def next-state* nil)
(defmulti next-state*
  (fn [computer {:keys [op-type] :as op}]
    op-type))

(defmethod next-state* :binary
  [computer {opf :op [p0 p1 dest] :params :as op}]
  (update
    (write computer dest (opf p0 p1))
    :instr-ptr
    +
    4))

(defn int->bool
  [x]
  (if (zero? x) false true))

(defn should-jump?
  [opc jump?]
  (if (= opc :op/jump-if)
    (int->bool jump?)
    (not (int->bool jump?))))


(defmethod next-state* :jump
  [computer {opc :op [jump? location] :params :as op}]
  (if (should-jump? opc jump?) 
    (assoc computer :instr-ptr location)
    (update computer :instr-ptr + 3)))

(defmethod next-state* :io
  [computer {opc :op [location] :params :as op}]
  (update
    (if (= opc :op/input)
      (assoc computer :state :state/need-input :input location)
      (assoc computer :state :state/output :output location))
    :instr-ptr
    +
    2))

(defmethod next-state* :relative
  [computer {[delta] :params :as op}]
  (-> computer
      (update :rel-ptr + delta)
      (update :instr-ptr + 2)))

(defmethod next-state* :default
  [computer op]
  (assoc computer :state :state/halted))


(defn new-computer
  [program]
  {:instr-ptr 0
   :rel-ptr 0
   :state :state/running
   :program program})


(defn next-state
  [{:keys [program state instr-ptr rel-ptr] :as computer}]
  (if (= state :state/running)
    (let [op (parse-op program instr-ptr)
          params (read-params program rel-ptr op)]
      (next-state* computer (assoc op :params params)))
    computer))


(defn check-state
  [computer program state instr-ptr rel-ptr]
  (is (= program (:program computer)))
  (is (= state (:state computer)))
  (is (= instr-ptr (:instr-ptr computer)))
  (is (= rel-ptr (:rel-ptr computer))))


(deftest test-next-state
  (testing "binary ops"
    (testing "plus"
      (check-state (next-state (new-computer [1101 1 1 0 99]))
                   [2 1 1 0 99]
                   :state/running
                   4
                   0))
    (testing "mult"
      (check-state (next-state (new-computer [1102 3 4 0 99]))
                   [12 3 4 0 99]   
                   :state/running
                   4
                   0))
    (testing "less than"
      (check-state (next-state (new-computer [1107 8 9 0 99]))
                   [1 8 9 0 99]
                   :state/running
                   4
                   0)
      (check-state (next-state (new-computer [1107 8 8 0 99]))
                   [0 8 8 0 99]
                   :state/running
                   4
                   0)
      (check-state (next-state (new-computer [1107 9 8 0 99]))
                   [0 9 8 0 99]
                   :state/running
                   4
                   0)
      (check-state (next-state (new-computer [1107 9 8 0 99]))
                   [0 9 8 0 99]
                   :state/running
                   4
                   0))
    (testing "equals"
      (check-state (next-state (new-computer [1108 8 8 0 99]))
                   [1 8 8 0 99]
                   :state/running
                   4
                   0)
      (check-state (next-state (new-computer [1108 8 9 0 99]))
                   [0 8 9 0 99]
                   :state/running
                   4
                   0)))
  (testing "jump ops"
    (testing "jump if"
      (check-state (next-state (new-computer [1105 1 87 99]))
                   [1105 1 87 99]
                   :state/running
                   87
                   0)
      (check-state (next-state (new-computer [1105 0 87 99]))
                   [1105 0 87 99]
                   :state/running
                   3
                   0))
    (testing "jump if not"
      (check-state (next-state (new-computer [1106 1 87 99]))
                   [1106 1 87 99]
                   :state/running
                   3 
                   0)
      (check-state (next-state (new-computer [1106 0 87 99]))
                   [1106 0 87 99]
                   :state/running
                   87
                   0)))
  (testing "input"
    (let [prg [3 987]
          {:keys [input] :as s} (next-state (new-computer prg))]
      (check-state s prg :state/need-input 2 0)
      (is (= 987 input))))
  (testing "output"
    (let [prg [4 0]
          {:keys [output] :as s} (next-state (new-computer prg))]
      (check-state s prg :state/output 2 0)
      (is (= 4 (:output s))))
    (let [prg [104 8889]
          {:keys [output] :as s} (next-state (new-computer prg))]
      (check-state s prg :state/output 2 0)
      (is (= 8889 (:output s))))
    (let [prg [204 3 0 77]
          {:keys [output] :as s} (next-state (new-computer prg))]
      (check-state s prg :state/output 2 0)
      (is (= 77 (:output s)))))
  (testing "rel ptr set"
    (check-state (next-state (new-computer [109 20]))
                 [109 20]
                 :state/running
                 2
                 20)
    (check-state (next-state (new-computer [209 2 89]))
                 [209 2 89]
                 :state/running
                 2
                 89)
    (check-state (next-state (new-computer [9 0]))
                 [9 0]
                 :state/running
                 2
                 9)))


(defn run
  [{:keys [state] :as computer}]
  (if (= :state/running state)
    (recur (next-state computer))
    computer))


(deftest test-run
  (testing "return on halt"
    (check-state (run (new-computer [109 2 1101 3 4 0 99]))
                 [7 2 1101 3 4 0 99]
                 :state/halted
                 6 
                 2))
  (testing "return on need input"
    (check-state (run (new-computer [109 2 1101 3 4 0 3 0 99]))
                 [7 2 1101 3 4 0 3 0 99]
                 :state/need-input
                 8 
                 2))
  (testing "return on output"
    (check-state (run (new-computer [109 2 1101 3 4 0 4 0 99]))
                 [7 2 1101 3 4 0 4 0 99]
                 :state/output
                 8 
                 2)))


(defn resume-with-input
  [{s :state location :input :as computer} value]
  (assert (= s :state/need-input))
  (-> computer
      (update :program assoc location value)
      (dissoc :input)
      (assoc :state :state/running)
      (run)))

(deftest test-resume-with-input
  (check-state (-> (new-computer [109 2 3 0 99])
                   (run)
                   (resume-with-input 87))
               [87 2 3 0 99]
               :state/halted
               4
               2))


(defn get-output
  [{:keys [state output] :as computer}]
  (assert (= state :state/output))
  output)


(defn resume-from-output
  [{:keys [state] :as computer}]
  (-> computer
      (dissoc :output)
      (assoc :state :state/running)
      (run)))


(deftest test-output
  (let [s0 (run (new-computer [3 0 4 0 99]))
        s1 (resume-with-input s0 89)
        out (get-output s1)
        s2 (resume-from-output s1)]
    (check-state s0 [3 0 4 0 99] :state/need-input 2 0)
    (check-state s1 [89 0 4 0 99] :state/output 4 0)
    (check-state s2 [89 0 4 0 99] :state/halted 4 0)
    (is (= 89 out))))


(defn parse-program
  [prg]
  (into []
        (comp (map string/trim)
              (map #(Long/valueOf %)))
        (string/split prg #",")))


(defn read-program
  [file]
  (parse-program (slurp (io/resource file))))


(deftest test-day2-examples
  (testing "step by step example"
    (let [s0 (next-state (new-computer (parse-program "1,9,10,3,2,3,11,0,99,30,40,50")))
          s1 (next-state s0)
          s2 (next-state s1)]
      (check-state s0
                   [1 9 10 70 2 3 11 0 99 30 40 50]
                   :state/running
                   4
                   0)
      (check-state s1
                   [3500 9 10 70 2 3 11 0 99 30 40 50]
                   :state/running
                   8
                   0)
      (check-state s2
                   [3500 9 10 70 2 3 11 0 99 30 40 50]
                   :state/halted
                   8
                   0)))
  (testing "fourth short example"
    (check-state (run (new-computer [1 1 1 4 99 5 6 0 99]))
                 [30 1 1 4 2 5 6 0 99]
                 :state/halted
                 8
                 0))
  (testing "part 1"
    (let [prg (-> (read-program "day_02_part1.txt")
                  (assoc 1 12)
                  (assoc 2 2))
          {:keys [program state]} (run (new-computer prg))]
      (is (= 3306701 (first program)))
      (is (= :state/halted state))))
  (testing "part 2"
    (let [prg (-> (read-program "day_02_part1.txt")
                  (assoc 1 76)
                  (assoc 2 21))
          {:keys [program state]} (run (new-computer prg))]
      (is (= 19690720 (first program)))
      (is (= :state/halted state)))))


(defn wrap-collect-output
  ([computer] (wrap-collect-output computer []))
  ([{:keys [state output] :as computer} coll]
   (case state
     :state/halted coll
     :state/output (recur (resume-from-output computer) (conj coll output))
     :state/running (recur (run computer) coll)
     (throw (ex-info "Shouldn't be here" computer)))))

(deftest test-day5
  (testing "part1"
    (is (= 7566643 (-> (new-computer (read-program "day_05.txt"))
                       (run)
                       (resume-with-input 1)
                       (wrap-collect-output)
                       (last)))))
  (testing "part2"
    (let [run-part2 (fn [program input]
                      (-> (new-computer program)
                          (run)
                          (resume-with-input input)
                          (wrap-collect-output)))]
      (testing "compare examples"
        (is (= [0] (run-part2 (parse-program "3,9,8,9,10,9,4,9,99,-1,8") 7)))
        (is (= [1] (run-part2 (parse-program "3,9,8,9,10,9,4,9,99,-1,8") 8)))
        (is (= [0] (run-part2 (parse-program "3,3,1107,-1,8,3,4,3,99") 8)))
        (is (= [1] (run-part2 (parse-program "3,3,1107,-1,8,3,4,3,99") 7))))
      (testing "jump examples"
        (let [prg (parse-program "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")]
          (is (= [0] (run-part2 prg 0)))
          (is (= [1] (run-part2 prg 1)))
          (is (= [1] (run-part2 prg 2)))
          (is (= [1] (run-part2 prg -1)))))
      (testing "last example"
        (is (= [999] (run-part2 (parse-program "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")
                                7)))
        (is (= [1000] (run-part2 (parse-program "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")
                                 8)))
        (is (= [1001] (run-part2 (parse-program "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")
                                 9))))) 
    (is (= 9265694 (-> (new-computer (read-program "day_05.txt"))
                       (run)
                       (resume-with-input 5)
                       (wrap-collect-output)
                       last)))))


(defn straight-amp
  [program phase-settings]
  (reduce (fn [input setting]
            (-> (new-computer program)
                (run)
                (resume-with-input setting)
                (resume-with-input input)
                (get-output)))
          0
          phase-settings))



(deftest test-day7
  (testing "part1, last example"
    (is (= 43210 (straight-amp (parse-program "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
                               [4 3 2 1 0])))
    (is (= 54321 (straight-amp (parse-program "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0")
                               [0 1 2 3 4])))
    (is (= 65210 (straight-amp (parse-program "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")
                               [1 0 4 3 2]))))
  
  )



(comment
  (straight-amp (parse-program "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")
                [1 0 4 3 2]
                
                )
  (-> (parse-program "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
      (new-computer)
      (run)
      (resume-with-input 0)
      (resume-with-input 4321)
      )

  ;A, 3
  ;B, 43
  ;C, 432
  ;D, 4321


  (prn (-> (read-program "day_09.txt")
           (new-computer)
           (run)
           (resume-with-input 1)
           (wrap-collect-output)
           ))
 ; day9 part 1 is not 203!
  )




; day 5 says that, "Parameters that an instruction writes to will never be in immediate mode"


