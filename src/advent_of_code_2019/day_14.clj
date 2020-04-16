(ns advent-of-code-2019.day-14
  (:require [clojure.test :refer :all]
            [instaparse.core :as insta]))


(def parser
  (insta/parser "<REACT_LIST> = REACT | REACT <'\n'> REACT_LIST
                REACT = CHEM_LIST <SPACE> <'=>'> <SPACE> CHEM 
                <CHEM_LIST> = CHEM | CHEM <','> <SPACE> CHEM_LIST 
                CHEM = QUANTITY <' '+> NAME 
                NAME = #'[A-Z]+'
                QUANTITY = #'[1-9][0-9]*'
                SPACE = ' '+"))


(defn format-chem
  [[_ [_ q] [_ n]]]
  [n (Long/valueOf q)])


(def parser-xform (comp (map (fn [[_ & chems]]
                               (map format-chem chems)))
                        (map (juxt last butlast))
                        (map (fn [[[chem mult] deps]]
                               [chem {:mult mult
                                      :deps deps}]))))


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
  (parse-reactions test-reactions-1)
  (parse-reactions test-reactions-2)
  (parse-reactions test-reactions-3)
  (parse-reactions test-reactions-4)
  (parse-reactions test-reactions-5)
  (parse-reactions input-reaction))


(deftest test-chem-RHS-count
  (let [get-RHS #(into [] (comp  parser-xform (map first)) (parser %))]
    (is (apply distinct? (get-RHS test-reactions-1)))
    (is (apply distinct? (get-RHS test-reactions-2)))
    (is (apply distinct? (get-RHS test-reactions-3)))
    (is (apply distinct? (get-RHS test-reactions-4)))
    (is (apply distinct? (get-RHS test-reactions-5)))
    (is (apply distinct? (get-RHS input-reaction)))))


(defn ore-deps?
  [deps]
  (some (fn [[chem-name amount]] (= "ORE" chem-name)) deps))


(deftest test-ore-always-alone?
  (let [get-ore-deps #(into []
                            (comp parser-xform
                                  (map (comp :deps second))
                                  (filter ore-deps?))
                            (parser %))]
    (is (every? #(= 1 (count %)) (get-ore-deps test-reactions-1)))
    (is (every? #(= 1 (count %)) (get-ore-deps test-reactions-2)))
    (is (every? #(= 1 (count %)) (get-ore-deps test-reactions-3)))
    (is (every? #(= 1 (count %)) (get-ore-deps test-reactions-4)))
    (is (every? #(= 1 (count %)) (get-ore-deps test-reactions-5)))
    (is (every? #(= 1 (count %)) (get-ore-deps input-reaction)))))


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


(defn initial-state
  [reactions]
  {:deps {"FUEL" 1}
   :final-deps {}
   :reactions reactions})


(defn next-dep
  [state]
  (first (:deps state)))


(defn remove-dep 
  [state chem-name]
  (update state :deps dissoc chem-name))


(defn add-deps*
  [deps new-deps]
  (reduce (fn [deps [chem-name amount]]
            (update deps chem-name (fnil + 0) amount))
          deps
          new-deps))


(defn add-deps 
  [state deps]
  (update state :deps add-deps* deps))


(defn add-final-deps
  [state deps]
  (update state :final-deps add-deps* deps))


(deftest test-state-changes
  (let [start (initial-state (parse-reactions test-reactions-1))]
    (is (= ["FUEL" 1] (next-dep start)))
    (is (= {} (:deps (remove-dep start "FUEL"))))
    (is (= {"A" 30 "B" 20} (-> (remove-dep start "FUEL")
                               (add-deps [["A" 10]])
                               (add-deps [["A" 20] ["B" 10]])
                               (add-deps [["B" 10]])
                               (add-deps [["C" 1] ["D" 3]])
                               (remove-dep "C")
                               (remove-dep "D")
                               :deps)))))

(defn mult-deps
  [scalar deps]
  (mapv (fn [[chem-name amount]]
          [chem-name (* scalar amount)])
        deps))


(defn ore-deps?
  [deps]
  (some (fn [[chem-name amount]] (= "ORE" chem-name)) deps))


(defn step
  [{:keys [reactions] :as state}]
  (let [[chem-name amount :as nd] (next-dep state)
        {react-quantity :mult react-deps :deps} (get reactions chem-name)
        next-state (remove-dep state chem-name)]
    (if (ore-deps? react-deps)
      (add-final-deps next-state [nd])
      (add-deps next-state (mult-deps (calc-reaction-count amount react-quantity) react-deps)))))


(defn final-step
  [{:keys [final-deps reactions] :as state}]
  (reduce (fn [ore-amount [chem-name amount]]
            (let [{react-quantity :mult [[_ dep-qty]] :deps} (get reactions chem-name)]
              (+ ore-amount (* (calc-reaction-count amount react-quantity) dep-qty))))
          0
          final-deps))


(defn find-ore-amount
  [reactions]
  (loop [state (initial-state reactions)]
    (if (empty? (:deps state))
      (final-step state)
      (recur (step state)))))


(deftest test-find-ore-amount
  (is (= 31 (find-ore-amount (parse-reactions test-reactions-1))))
  (is (= 165 (find-ore-amount (parse-reactions test-reactions-2))))
  (is (= 13312 (find-ore-amount (parse-reactions test-reactions-3))))
  (is (= 180697 (find-ore-amount (parse-reactions test-reactions-4))))
  #_(is (= 2210736 (find-ore-amount (parse-reactions test-reactions-5))))
  )


#_(-> (initial-state (parse-reactions test-reactions-2))
      (step)
      (step)
      (step)
      (step)
      (dissoc :reactions)
      )


#_(reverse-reactions {"FUEL" 1} (parse-reactions test-reactions-2))
#_(reverse-reactions {"AB" 2 "BC" 3 "CA" 4} (parse-reactions test-reactions-2))
#_(reverse-reactions {"A" 10, "B" 23, "C" 37} (parse-reactions test-reactions-2))
(defn reverse-reactions
  [outs reactions]
  (reduce (fn [chems [chem-name qty]]
            (let [{react-qty :mult ins :deps} (get reactions chem-name)
                  step-chems (if (ore-deps? ins)
                               {chem-name qty}
                               (into {} (mult-deps (calc-reaction-count qty react-qty) ins)))]
              (merge-with + chems step-chems)))
          {}
          outs))




#_(prn (reverse-reactions {"FUEL" 1} (parse-reactions test-reactions-1)))
#_(reverse-reactions {"A" 7, "E" 1} (parse-reactions test-reactions-1))
#_(reverse-reactions {"A" 14, "D" 1} (parse-reactions test-reactions-1))
#_(reverse-reactions {"A" 21, "C" 1} (parse-reactions test-reactions-1))
#_(reverse-reactions {"A" 28, "B" 1} (parse-reactions test-reactions-1))



(defn convert-to-ore
  [chems reactions]
  (reduce (fn [ore-qty [chem-name qty]]
            (let [{react-qty :mult [[_ dep-qty]] :deps} (get reactions chem-name)]
              (+ ore-qty (* dep-qty (calc-reaction-count qty react-qty)))))
          0
          chems))

(defn unreact
  [reactions]
  (loop [chems {"FUEL" 1}]
    (let [next-chems (reverse-reactions chems reactions)]
      (if (= chems next-chems)
        (convert-to-ore chems reactions)
        (recur next-chems)))))

#_ (prn (unreact (parse-reactions test-reactions-5)))

(deftest test-unreact
  (is (= 31 (unreact (parse-reactions test-reactions-1))))
  (is (= 165 (unreact (parse-reactions test-reactions-2))))
  (is (= 13312 (unreact (parse-reactions test-reactions-3))))
  (is (= 180697 (unreact (parse-reactions test-reactions-4))))
  #_(is (= 2210736 (unreact (parse-reactions test-reactions-5))))
  
  )


#_(prn (reverse-reactions {"FUEL" 1} (parse-reactions test-reactions-5)))
#_(prn (reverse-reactions {"BHXH" 6, "KTJDG" 18, "WPTQ" 12, "PLWSL" 7, "FHTLT" 31, "ZDVW" 37} (parse-reactions test-reactions-5)))
#_(prn (reverse-reactions {"ZLQW" 99, "BHXH" 6, "BMBT" 33, "KTJDG" 126, "MZWV" 114, "XCVML" 18, "XMNCP" 576, "LTCX" 370, "RJRHP" 119, "WPTQ" 519}
                          (parse-reactions test-reactions-5)))
#_(prn (reverse-reactions {"XDBXC" 5151, "LTCX" 34, "VRPVC" 431, "BHXH" 475, "KTJDG" 186, "MZWV" 288, "XCVML" 1004, "BMBT" 650}
                          (parse-reactions test-reactions-5)))
#_(prn (reverse-reactions {"VRPVC" 32981, "CNZTR" 69552, "BHXH" 3048, "KTJDG" 3201}
                          (parse-reactions test-reactions-5)))






(defn solve-part-1
  [reactions]
  (loop [reqs (queue ["FUEL" 1])
         inventory {}]
    (let [[chem-name chem-qty] (peek reqs)
          {react-qty :mult ins :deps} (get reactions chem-name)
          react-cnt (calc-reaction-count chem-qty react-qty)
          step-reqs (mult-deps react-cnt ins)]

      )))


(defn next-req
  [state]
  (peek (:reqs state)))


(defn pop-req
  [state]
  (update state :reqs pop))


(defn queue-req
  [state chem-name qty]
  (update state :reqs conj [chem-name qty]))


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
  [reactions]
  (-> {:reqs clojure.lang.PersistentQueue/EMPTY
       :inventory {}
       :reactions reactions 
       :ore 0}
      (queue-req "FUEL" 1)))


(deftest test-state-manip
  (testing "reqs queue"
    (let [s0 (init-state {})
          r0 (next-req s0)
          s1 (queue-req s0 "A" 3)
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


(defn pr-state
  [state]
  (clojure.pprint/pprint (update state :reqs seq))
  state)


; Step algorithm
; pull req from queue
; pull as much from inventory as possible 
; find reaction to produce the rest
; put any overage into the inventory
; put reaction requirements into queue

; special case for ore?

; stop?


#_(-> (init-state (parse-reactions test-reactions-2))
      (step)
      (step)
      (step)
      (step)
      pr-state
      (step)
      pr-state)
(defn step
  [state]
  (let [[req-chem-name req-qty] (next-req state)]
    (if (= "ORE" req-chem-name)
      (-> state (add-ore req-qty) (pop-req))
      (let [inv-qty (get-inventory state req-chem-name)
            gen-qty (- req-qty inv-qty)
            inv-qty-delta (- (min req-qty inv-qty))]
        (if (<= gen-qty 0)
          ; no reaction needed
          (-> state
              (update-inventory req-chem-name inv-qty-delta)
              (pop-req))
          (let [{react-qty :mult react-reqs :deps} (get-reaction state req-chem-name)
                react-cnt (calc-reaction-count gen-qty react-qty)
                new-reqs (mult-deps react-cnt react-reqs)
                extra-qty (- react-qty gen-qty)
                inv-qty-delta (+ inv-qty-delta extra-qty)]
            (-> state
                (update-inventory req-chem-name inv-qty-delta)
                (queue-reqs new-reqs)
                (pop-req))))))))


(defn find-ore-amount
  [reactions]
  (loop [state (init-state reactions)]
    (if (empty-reqs? state)
      (:ore state)
      (recur (step state)))))


(deftest test-find-ore-amount
  (is (= 31 (find-ore-amount (parse-reactions test-reactions-1))))
  (is (= 165 (find-ore-amount (parse-reactions test-reactions-2))))
  (is (= 13312 (find-ore-amount (parse-reactions test-reactions-3))))
  (is (= 180697 (find-ore-amount (parse-reactions test-reactions-4))))
  )


