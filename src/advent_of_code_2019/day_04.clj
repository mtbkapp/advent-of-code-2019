(ns advent-of-code-2019.day-04
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer :all]))



(defn has-adjacent-digits?
  [n]
  (some (fn [[d0 d1]]
          (= d0 d1))
        (partition 2 1 (str n))))


(defn is-increasing?
  [n]
  (->> (str n)
       (map #(Long/valueOf (str %)))
       (partition 2 1)
       (every? (fn [[d0 d1]] (<= d0 d1)))))


(def min-n 235741)
(def max-n 706948)


#_(solve-part-1)
(defn solve-part-1
  []
  (count
    (filter #(and (has-adjacent-digits? %)
                  (is-increasing? %))
            (range min-n max-n))))


(defn has-adjacent-digits-part-2?
  [n]
  (-> (frequencies (str n))
      vals
      set
      (contains? 2)))

#_(solve-part-2)
(defn solve-part-2
  [] 
  (count
    (filter #(and (has-adjacent-digits-part-2? %)
                  (is-increasing? %))
            (range min-n max-n))))
