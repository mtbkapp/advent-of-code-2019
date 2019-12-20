(ns advent-of-code-2019.intcode
  (:require [clojure.test :refer :all]))


(defn wrap-int-bool
  [f]
  (fn [x y]
    (bool->int (f x y))))

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
    (= :jump op-type) (let [[mode] param-modes
                            [a0 jump-ptr] args]
                        [(get-param program rel-ptr a0 mode)
                         jump-ptr])
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
    (let [prg [5 0 876 99]]
      (is (= [5 876]
             (read-params prg 1 (parse-op prg 0)))))
    (let [prg [105 0 876 99]]
      (is (= [0 876]
             (read-params prg 1 (parse-op prg 0)))))
    (let [prg [105 0 876 99]]
      (is (= [0 876]
             (read-params prg 1 (parse-op prg 0)))))
    (let [prg [205 2 876 99]]
      (is (= [99 876]
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


(defn update-instr-ptr
  [computer delta]
  )

(defn write
  [computer dest v]
  ; TODO expand memory when needed
  (update computer
          :program 
          assoc
          dest
          v))

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
  (if (zero? x) 0 1))

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
  [computer {opc :op [output] :params :as op}]
  (update
    (if (= opc :op/input)
      (assoc computer :state :state/need-input)
      (assoc computer :state :state/output :output output))
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
    
    )
  (testing "input"
    
    )
  (testing "output"
    
    )
  (testing "rel ptr set"
    ; rel-ptr = param
    ; or
    ; rel-ptr = rel-ptr + param
    ))


(defn run
  [computer]
  ) 
