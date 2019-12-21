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
        [op0 op1 mode0 mode1 mode2] (reverse (str opcode))
        {:keys [param-count] :as op} (get opcodes (str (or op1 "0") op0))
        param-modes (->> [mode0 mode1 mode2]
                         (map #(get param-modes % :mode/position) )
                         (take param-count))
        args (take param-count [a0 a1 a2])]
    (assoc op :param-modes param-modes :args args)))


(defn get-param
  [program rel-ptr arg mode]
  (case mode
    :mode/position (get program arg 0)
    :mode/relative (get program (+ rel-ptr arg) 0)
    :mode/immediate arg))


(defn read-params
  [program rel-ptr {opc :op :keys [op-type args param-modes param-count] :as op}]
  (cond 
    (= :binary op-type) (let [[m0 m1 m2] param-modes
                              [a0 a1 dest] args]
                          [(get-param program rel-ptr a0 m0)
                           (get-param program rel-ptr a1 m1)
                           (if (= :mode/relative m2) (+ rel-ptr dest) dest)])
    (= :jump op-type) (let [[m0 m1] param-modes
                            [a0 a1] args]
                        [(get-param program rel-ptr a0 m0)
                         (get-param program rel-ptr a1 m1)])
    (= :op/output opc) [(get-param program
                                   rel-ptr
                                   (first args)
                                   (first param-modes))]
    (= :op/input opc) (let [[mode] param-modes
                            [a] args]
                        [(if (= mode :mode/position) a (+ rel-ptr a))])
    (= :op/rel-set opc) [(get-param program
                                    rel-ptr
                                    (first args)
                                    (first param-modes))]
    :else []))


(defn expand-memory
  [computer max-i]
  (update computer
          :program
          (fn [prg]
            (if (< max-i (count prg))
              prg
              (into prg (repeat (- (inc max-i) (count prg)) 0))))))


(defn write
  [computer dest v]
  (-> (expand-memory computer dest)
      (update :program 
              assoc
              dest
              v)))


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
      (try
        (next-state* computer (assoc op :params params))
        (catch Exception ex
          (throw (ex-info "Error calling next-state*" {:state computer} ex)))))
    computer))


(defn check-state
  [computer program state instr-ptr rel-ptr]
  (is (= program (:program computer)))
  (is (= state (:state computer)))
  (is (= instr-ptr (:instr-ptr computer)))
  (is (= rel-ptr (:rel-ptr computer))))


(defn run
  [{:keys [state] :as computer}]
  (if (= :state/running state)
    (recur (next-state computer))
    computer))


(defn resume-with-input
  [{s :state location :input :as computer} value]
  (assert (= s :state/need-input))
  (-> computer
      (write location value)
      (dissoc :input)
      (assoc :state :state/running)
      (run)))


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


(defn wrap-collect-output
  ([computer] (wrap-collect-output computer []))
  ([{:keys [state output] :as computer} coll]
   (case state
     :state/halted coll
     :state/output (recur (resume-from-output computer) (conj coll output))
     :state/running (recur (run computer) coll)
     (throw (ex-info "Shouldn't be here" computer)))))


(defn parse-program
  [prg]
  (into []
        (comp (map string/trim)
              (map #(Long/valueOf %)))
        (string/split prg #",")))


(defn read-program
  [file]
  (parse-program (slurp (io/resource file))))

