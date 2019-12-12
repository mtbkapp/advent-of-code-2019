(ns advent-of-code-2019.day-08
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer :all]))



(defn read-input
  []
  (->> (io/resource "day_08.txt")
       slurp
       seq
       butlast))


(defn parse-layers
  ([input] (parse-layers input [25 6]))
  ([input [w h]]
   (partition (* w h) input)))


#_(solve-part-1)
(defn solve-part-1
  []
  (let [{ones \1 twos \2} (->> (read-input)
                               (parse-layers)
                               (sort-by (comp #(get % \0) frequencies))
                               (first)
                               (frequencies))]
    (* ones twos)))


(defn compose-pixels
  [bottom top]
  (if (= top \2)
    bottom
    top))


(defn compose-layers
  [layers]
  (reduce (fn [composite layer]
            (map compose-pixels composite layer))
          (reverse layers)))


(deftest test-compose-layers
  (is (= [\0 \1 \1 \0]
         (compose-layers (parse-layers (seq "0222112222120000") [2 2])))))

(defn render
  [layer]
  (doseq [row (partition 25 layer)]
    (doseq [px row]
      (print (if (= px \1) "W" " ")))
    (println)))


#_(solve-part-2) ;LEJKC
(defn solve-part-2
  []
  (render (compose-layers (parse-layers (read-input)))))
