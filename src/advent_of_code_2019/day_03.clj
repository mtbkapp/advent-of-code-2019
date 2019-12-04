(ns advent-of-code-2019.day-03
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.test :refer :all]))



(defn parse-input
  [input]
  (map (comp #(map (juxt first
                         (fn [s] (Long/valueOf (subs s 1))))
                   %)
             #(string/split % #","))
       (string/split-lines input)))


(deftest test-parse-input
  (is (= [[[\R 8] [\U 5] [\L 5] [\D 3]]
          [[\U 7] [\R 6] [\D 4] [\L 4]]]
         (parse-input "R8,U5,L5,D3\nU7,R6,D4,L4"))))


(defn read-input
  []
  (slurp (io/resource "day_03.txt")))


(defn vec-add
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])


(defn scale-vec
  [[x y] s]
  [(* x s) (* y s)])


(def dir->unit-vec
  {\R [1 0]
   \L [-1 0]
   \U [0 1]
   \D [0 -1]})


(defn make-line
  [pos [dir cnt]]
  (map (fn [s] 
         (vec-add pos (scale-vec (dir->unit-vec dir) s)))
       (range 1 (inc cnt))))


(defn rasterize-wire
  [wire]
  (first (reduce (fn [[points pos] move]
                   (let [line (make-line pos move)]
                     [(into points line) (last line)]))
                 [#{} [0 0]]
                 wire)))


(defn mdist
  [[x y]]
  (+ (Math/abs x) (Math/abs y)))


#_(solve-part-1)
(defn solve-part-1
  ([] (solve-part-1 (parse-input (read-input))))
  ([wires]
   (->> wires 
       (map rasterize-wire)
       (apply sets/intersection)
       (map mdist)
       (reduce min))))


(deftest test-closest-intersection
  (is (= 6 (solve-part-1 (parse-input "R8,U5,L5,D3\nU7,R6,D4,L4"))))
  (is (= 159 (solve-part-1 (parse-input "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"))))
  (is (= 135 (solve-part-1 (parse-input "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))))


#_(with-open [wr (java.io.PrintWriter. (io/writer "raster.txt"))]
    (binding [*out* wr]
      (render-points (->> "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
                          (parse-input)
                          (map rasterize-wire)
                          (apply into)))))

(defn render-points
  [points]
  (let [ps (set points)
        xs (map first points)
        ys (map second points)
        border 5
        min-x (- (reduce min xs) border)
        max-x (+ (reduce max xs) border)
        min-y (- (reduce min ys) border)
        max-y (+ (reduce max ys) border)]
    (prn [[min-x min-y] [max-x max-y]])
    (doseq [y (range min-y max-y)]
      (doseq [x (range min-x max-x)]
        (print (cond (contains? ps [x y]) "X"
                     (= [0 0] [x y]) "0"
                     :else ".")))
      (println))))


(defn build-dist-map 
  [wire]
  (first (reduce (fn [[_ spos :as acc] [dir cnt]]
                   (reduce (fn [[points pos dist] s]
                             (let [p (vec-add spos (scale-vec (dir->unit-vec dir) s))]
                               (if (contains? points p)
                                 [points p (inc (get points p))]
                                 [(assoc points p dist) p (inc dist)])))
                           acc 
                           (range 1 (inc cnt))))
                 [{} [0 0] 1]
                 wire)))


(deftest dist-map-vs-rasterize
  (let [[w0 w1] (parse-input "R8,U5,L5,D3\nU7,R6,D4,L4")]
    (is (= (rasterize-wire w0) (set (keys (build-dist-map w0)))))
    (is (= (rasterize-wire w1) (set (keys (build-dist-map w1))))))
  (let [[w0 w1] (parse-input (read-input))]
    (is (= (rasterize-wire w0) (set (keys (build-dist-map w0)))))
    (is (= (rasterize-wire w1) (set (keys (build-dist-map w1)))))))


(defn dist-maps->intersections
  [dist-maps]
  (->> (map (comp set keys) dist-maps)
       (apply sets/intersection)))

(defn dist-to
  [w0 w1 p]
  (+ (get w0 p) (get w1 p)))


#_(clojure.pprint/pprint (solve-part-2 "R8,U5,L5,D3\nU7,R6,D4,L4"))
#_(clojure.pprint/pprint (= (solve-part-2)
                            (apply sets/intersection
                                   (map rasterize-wire (parse-input (read-input))))))
(defn solve-part-2
  ([] (solve-part-2 (read-input)))
  ([input]
   (let [[w0 w1 :as dist-maps] (map build-dist-map (parse-input input))]
     (->> (dist-maps->intersections dist-maps)
          #_(map (partial dist-to w0 w1))
          #_(reduce min)))))


(deftest test-solve-part2
  (is (= 30 (solve-part-2 "R8,U5,L5,D3\nU7,R6,D4,L4")))
  (is (= 610 (solve-part-2 "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83")))
  (is (= 410 (solve-part-2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7" ))))


; start over
; 1. start at 0,0


(let [[w0 w1] (parse-input "R8,U5,L5,D3\nU7,R6,D4,L4")]
  )

#_(render-dist-map (build-dist-map (first (parse-input "R8,U5,L5,D7,R3,U9\nU7,R6,D4,L4"))) "raster.txt")
#_(render-dist-map (build-dist-map (first (parse-input "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"))) "raster.txt")
(defn render-dist-map
  [dist-map file]
  (with-open [wr (java.io.PrintWriter. (io/writer file))]
    (binding [*out* wr]
      (let [ps (set (keys dist-map))
            xs (map first ps)
            ys (map second ps)
            border 5
            min-x (- (reduce min xs) border)
            max-x (+ (reduce max xs) border)
            min-y (- (reduce min ys) border)
            max-y (+ (reduce max ys) border)]
        (prn [[min-x min-y] [max-x max-y]])
        (doseq [y (range min-y max-y)]
          (doseq [x (range min-x max-x)]
            (print (cond (contains? ps [x y]) (str "[" (format "%3d" (get dist-map [x y])) "]") 
                         (= [0 0] [x y]) "[ o ]"
                         :else "[   ]")))
          (println))))))
