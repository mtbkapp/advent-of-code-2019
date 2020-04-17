(ns advent-of-code-2019.day-14
  (:require [clojure.test :refer :all]
            [instaparse.core :as insta]))


(def parser
  (insta/parser "<REACT_LIST> = REACT | REACT <'\n'> REACT_LIST
                REACT = CHEM_LIST <SPACE> <'=>'> <SPACE> CHEM 
                <CHEM_LIST> = CHEM | CHEM <','> <SPACE> CHEM_LIST 
                CHEM = QUANTITY <SPACE> NAME 
                NAME = #'[A-Z]+'
                QUANTITY = #'[1-9][0-9]*'
                SPACE = ' '+"))


(defn format-chem
  [[_ [_ q] [_ n]]]
  [n (Long/valueOf q)])


(def parser-xform (comp (map (fn [[_ & chems]]
                               (map format-chem chems)))
                        (map (juxt last butlast))
                        (map (fn [[[chem qty] reqs]]
                               [chem {:qty qty
                                      :reqs reqs}]))))


(defn parse-reactions
  [text]
  (into {} parser-xform (parser text)))


(def test-reactions-1
  "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL")


(def test-reactions-2
  "9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL")


(def test-reactions-3
  "157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")


(def test-reactions-4
  "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF")


(def test-reactions-5
  "171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX")


(def input-reaction
  "3 DJDNR => 1 ZCMR
7 VWJH => 5 ZPGT
5 BHZP => 2 DJDNR
6 KCNKC, 19 MZWS => 4 PKVJF
21 GXSHP, 1 TWGP, 3 BGCW => 1 XHRWR
12 DZGWQ, 2 XRDL, 3 XNVT => 2 FTMC
7 VWJH, 33 BGCW, 1 TBVC => 9 DSDP
1 NMTGB, 4 KCNKC, 5 SBSJ, 4 MCZDZ, 7 DLCP, 2 GRBZF, 1 CLKP, 10 VQHJG => 6 DVCR
7 ZCMR => 9 VNTF
2 VNTF, 1 GKMN => 1 TZWBH
6 QMFV, 7 GRBZF => 7 RHDZ
8 PKVJF => 9 NJQH
110 ORE => 9 GZTS
4 DJDNR, 7 SFHV => 8 KQFH
1 ZTCZ, 5 LZFBP => 7 VWPMZ
2 GKMN, 6 TZWBH, 1 GXSHP => 1 MJHJH
2 DLCP, 4 NGJRN => 3 GRBZF
2 DJDNR, 1 GSRBL => 4 VWJH
7 RMQX => 3 SFHV
1 GZTS => 7 GSRBL
3 GZTS, 1 SFHV => 3 QLXCS
10 SFHV => 3 MKTHL
2 DJDNR, 2 BGCW, 4 FSTJ => 3 GKMN
2 KQFH, 7 GSRBL => 7 TWGP
22 RHDZ, 22 DZGWQ, 2 NGJRN, 14 XHRWR, 21 VWPMZ, 15 ZPXHM, 26 BHZP => 8 BPHZ
1 QLXCS => 6 ZBTS
12 DLCP, 9 DSDP => 9 ZPXHM
1 VNTF => 5 ZBTX
2 TZWBH, 2 JCDW => 1 CPLG
1 XHRWR, 7 FSTJ, 5 DZGWQ => 4 NGJRN
179 ORE => 3 RMQX
1 DSDP => 1 MZWS
140 ORE => 8 BHZP
1 LZFBP, 4 DZGWQ => 2 PMDK
1 GZTS => 1 GXSHP
10 CPLG, 8 MCZDZ => 5 ZTCZ
5 ZPGT, 4 THLBN, 24 GSRBL, 40 VNTF, 9 DVCR, 2 SHLP, 11 PMDK, 19 BPHZ, 45 NJQH => 1 FUEL
9 MKTHL => 7 KCNKC
5 NGJRN => 3 QMFV
1 ZTCZ, 6 VNTF => 2 VQHJG
5 FTMC, 5 ZBTX, 1 MJHJH => 1 CLKP
7 FSTJ => 6 DLCP
1 DSDP => 5 KTML
4 LZFBP, 8 MKTHL => 7 MCZDZ
1 SFHV => 1 DZGWQ
2 QLXCS => 4 ZMXRH
3 KQFH, 1 DJDNR => 7 TBVC
5 DSDP => 7 THLBN
9 BHZP, 1 VWJH => 6 BGCW
4 GXSHP => 6 JCDW
1 KQFH, 3 ZMXRH => 9 XNVT
6 TBVC => 4 GVMH
3 VWPMZ, 3 GRBZF, 27 MJHJH, 2 QMFV, 4 NMTGB, 13 KTML => 7 SHLP
1 GVMH => 2 FSTJ
2 VQHJG, 2 NJQH => 8 SBSJ
1 XNVT => 2 XRDL
2 KCNKC => 5 LZFBP
2 ZBTS, 8 DLCP => 4 NMTGB")


(deftest test-parser
  (testing "does parsing error?"
    (parse-reactions test-reactions-1)
    (parse-reactions test-reactions-2)
    (parse-reactions test-reactions-3)
    (parse-reactions test-reactions-4)
    (parse-reactions test-reactions-5)
    (parse-reactions input-reaction)))


; only one way to generate a chemical?
(deftest test-chem-RHS-count
  (let [get-RHS #(into [] (comp  parser-xform (map first)) (parser %))]
    (is (apply distinct? (get-RHS test-reactions-1)))
    (is (apply distinct? (get-RHS test-reactions-2)))
    (is (apply distinct? (get-RHS test-reactions-3)))
    (is (apply distinct? (get-RHS test-reactions-4)))
    (is (apply distinct? (get-RHS test-reactions-5)))
    (is (apply distinct? (get-RHS input-reaction)))))


(defn ore-reqs?
  [reqs]
  (some (fn [[chem-name amount]] (= "ORE" chem-name)) reqs))


; when ore is an input it's only the input
(deftest test-ore-always-alone?
  (let [get-ore-reqs #(into []
                            (comp parser-xform
                                  (map (comp :reqs second))
                                  (filter ore-reqs?))
                            (parser %))]
    (is (every? #(= 1 (count %)) (get-ore-reqs test-reactions-1)))
    (is (every? #(= 1 (count %)) (get-ore-reqs test-reactions-2)))
    (is (every? #(= 1 (count %)) (get-ore-reqs test-reactions-3)))
    (is (every? #(= 1 (count %)) (get-ore-reqs test-reactions-4)))
    (is (every? #(= 1 (count %)) (get-ore-reqs test-reactions-5)))
    (is (every? #(= 1 (count %)) (get-ore-reqs input-reaction)))))


(defn calc-reaction-count
  [dep-quantity react-quantity]
  (long (Math/ceil (/ (double dep-quantity) react-quantity))))


(deftest test-calc-reaction-count
  (is (= 2 (calc-reaction-count 2 1)))
  (is (= 1 (calc-reaction-count 1 1)))
  (is (= 1 (calc-reaction-count 1 3)))
  (is (= 1 (calc-reaction-count 2 3)))
  (is (= 1 (calc-reaction-count 3 3)))
  (is (= 2 (calc-reaction-count 4 3)))
  (is (= 2 (calc-reaction-count 5 3)))
  (is (= 2 (calc-reaction-count 6 3)))
  (is (= 3 (calc-reaction-count 7 3))))


(defn mult-reqs
  [scalar reqs]
  (mapv (fn [[chem-name amount]]
          [chem-name (* scalar amount)])
        reqs))


(defn next-req
  [state]
  (peek (:reqs state)))


(defn pop-req
  [state]
  (update state :reqs pop))


(defn queue-reqs
  [state reqs]
  (update state :reqs into reqs))


(defn empty-reqs?
  [state]
  (empty? (:reqs state)))


(defn get-inventory
  [state chem-name]
  (get-in state [:inventory chem-name] 0))


(defn update-inventory
  [state chem-name delta]
  (update-in state [:inventory chem-name] (fnil + 0) delta))


(defn get-reaction
  [state chem-name]
  (get-in state [:reactions chem-name]))


(defn add-ore
  [state qty]
  (update state :ore + qty))


(defn init-state
  ([reactions] (init-state reactions 1))
  ([reactions fuel-qty]
   (-> {:reqs clojure.lang.PersistentQueue/EMPTY
        :inventory {}
        :reactions reactions 
        :ore 0}
       (queue-reqs [["FUEL" fuel-qty]]))))


(deftest test-state-manip
  (testing "reqs queue"
    (let [s0 (init-state {})
          r0 (next-req s0)
          s1 (queue-reqs s0 [["A" 3]])
          r1 (next-req s1)
          r2 (-> s1 (pop-req) (next-req))
          r3 (-> s1 (pop-req) (pop-req) (next-req))]
      (is (= r0 ["FUEL" 1]))
      (is (= r1 ["FUEL" 1]))
      (is (= r2 ["A" 3]))
      (is (nil? r3))))
  (testing "inventory"
    (let [s0 (init-state {})
          i0 (get-inventory s0 "A")
          s1 (update-inventory s0 "A" 2)
          i1 (get-inventory s1 "A")
          s2 (update-inventory s1 "A" -1)
          i2 (get-inventory s2 "A")]
      (is (zero? i0))
      (is (= 2 i1))
      (is (= 1 i2)))))


(defn step
  [state]
  (let [[req-chem-name req-qty] (next-req state)]
    (if (= "ORE" req-chem-name)
      ; ore req? -> add to total
      (-> state (add-ore req-qty) (pop-req))
      (let [inv-qty (get-inventory state req-chem-name)
            gen-qty (- req-qty inv-qty)
            inv-qty-delta (- (min req-qty inv-qty))]
        (if (<= gen-qty 0)
          ; no reaction needed to supply req
          ; update inventory and return
          (-> state
              (update-inventory req-chem-name inv-qty-delta)
              (pop-req))
          ; need to find the reaction to meet the req
          ; queue new reqs
          ; update inventory amount for left overs
          (let [{react-qty :qty react-reqs :reqs} (get-reaction state req-chem-name)
                react-cnt (calc-reaction-count gen-qty react-qty)
                new-reqs (mult-reqs react-cnt react-reqs)
                extra-qty (- (* react-cnt react-qty) gen-qty)
                inv-qty-delta (+ inv-qty-delta extra-qty)]
            (-> state
                (update-inventory req-chem-name inv-qty-delta)
                (queue-reqs new-reqs)
                (pop-req))))))))


(defn find-ore-amount
  ([reactions] (find-ore-amount reactions 1))
  ([reactions fuel-qty]
   (loop [state (init-state reactions fuel-qty)]
     (if (empty-reqs? state)
       (:ore state)
       (recur (step state))))))


(deftest test-find-ore-amount
  (is (= 31 (find-ore-amount (parse-reactions test-reactions-1))))
  (is (= 165 (find-ore-amount (parse-reactions test-reactions-2))))
  (is (= 13312 (find-ore-amount (parse-reactions test-reactions-3))))
  (is (= 180697 (find-ore-amount (parse-reactions test-reactions-4))))
  (is (= 2210736 (find-ore-amount (parse-reactions test-reactions-5)))))


; part 2
(def ore-inv 1000000000000)

#_(prn (fuel-for-trillion-ore (parse-reactions input-reaction) 1000)) ; => {:max-fuel 4052920, :iters 20}
(defn fuel-for-trillion-ore
  "Find how much fuel can be made with 1 trillion units of ore. I previously
  found via trial and error that 5000000 fuel required more than 1 trillion ore
  and 4000000 required less than 1 million ore. This algorithm does a binary
  search for the amount of fuel that 1 trillion ore will produce."
  [reactions max-iters]
  (loop [mx 5000000
         fuel 4500000 
         mn 4000000
         n max-iters]
    (if (< 0 n)
      (if (= 1 (- mx mn)) 
        {:max-fuel mn :iters (- max-iters n)} 
        (case (compare ore-inv (find-ore-amount reactions fuel))
          -1 (recur fuel
                    (+ mn (quot (- fuel mn) 2))
                    mn
                    (dec n)) 
          0 {:max-fuel fuel :iters (- max-iters n)}
          1 (recur mx
                   (- mx (quot (- mx fuel) 2))
                   fuel
                   (dec n))))
      (throw (ex-info "Hit max iterations!" {:params [mx fuel mn n]})))))

