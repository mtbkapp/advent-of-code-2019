(ns advent-of-code-2019.day-13
  (:require [advent-of-code-2019.intcode :as intcode]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]
            [clojure.java.io :as io])
  (:import [com.googlecode.lanterna.terminal DefaultTerminalFactory]
           [java.io Closeable]
           [com.googlecode.lanterna
            SGR
            TextColor
            TextColor$ANSI
            TextCharacter
            TerminalSize]
           [com.googlecode.lanterna.input KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]))



(def tile-mapping
  {0 :tile/empty
   1 :tile/wall 
   2 :tile/block
   3 :tile/paddle
   4 :tile/ball})


(defn lchar
  [c ^TextColor color]
  (TextCharacter. c color TextColor$ANSI/BLACK (make-array SGR 0)))


(def tile-char-mapping
  {0 (lchar \space TextColor$ANSI/BLACK) ; empty 
   1 (lchar \| TextColor$ANSI/WHITE) ; wall
   2 (lchar \# TextColor$ANSI/GREEN) ; block
   3 (lchar \# TextColor$ANSI/GREEN) ; paddle
   4 (lchar \* TextColor$ANSI/BLUE)} ; ball
  )


(defn run-program 
  []
  (-> (intcode/read-program "day_13.txt")
      (intcode/new-computer)
      (intcode/wrap-collect-output)))


(deftest sanity-check
  (is (zero? (mod (count (run-program)) 3))))


; Solution to part 1
#_(count-blocks-at-exit) ; 452
(defn count-blocks-at-exit
  []
  (->> (run-program)
       (partition 3)
       (reduce (fn [screen [x y t]]
                 (assoc screen [x y] (get tile-mapping t)))
               {})
       (vals)
       (frequencies)
       :tile/block))



#_(screen-size) ; [42 25]
(defn screen-size
  []
  (->> (run-program)
       (partition 3)
       (reduce (fn [[mx my] [x y t]]
                 [(max mx x)
                  (max my y)])
               [0 0])))


(defn add-quarters
  [computer]
  (update computer :program assoc 0 2))


(def tile-char-mapping
  {0 (lchar \space TextColor$ANSI/BLACK) ; empty 
   1 (lchar \| TextColor$ANSI/WHITE) ; wall
   2 (lchar \# TextColor$ANSI/GREEN) ; block
   3 (lchar \- TextColor$ANSI/MAGENTA) ; paddle
   4 (lchar \* TextColor$ANSI/CYAN)} ; ball
  )


(defn put-msg
  [screen x y msg]
  (-> (.newTextGraphics screen)
      (.putString x y msg)))


(def last-score (atom nil))


(defn put-tile
  [screen x y tile]
  (if (= [-1 0] [x y])
    (do (put-msg screen 45 1 (str "Score: " tile))
        (reset! last-score tile)) 
    (.setCharacter screen x y (get tile-char-mapping tile))))


(defmacro with-screen
  [[screen cols rows rate] & body]
  `(let [term# (.createTerminal (doto (DefaultTerminalFactory.)
                                  (.setInitialTerminalSize (TerminalSize. ~cols ~rows))))
         ~screen (TerminalScreen. term#)]
     (try
       (.startScreen ~screen)
       ~@body
       (finally
         (.stopScreen ~screen)
         (.close term#)))))


(defn get-input
  [screen]
  (if-let [stroke (.pollInput screen)]
    (let [t (.getKeyType stroke)]
      (cond (= t KeyType/ArrowRight) 1
            (= t KeyType/ArrowLeft) -1 
            :else 0))
    0))


; solution strategy
; 1. change floor to be "wall" in program -> got lucky finding where that is 
;    specified
; 2. play game until all blocks are gone. Mostly just watch it go as fast as it 
;    can. Use paddle to get it out of a loop so it can get last few blocks on
;    its own
; 3. print score
; 
; last score = 21415
(defn -main
  [& args]
  (with-screen
    [screen 80 40]
    (.readInput screen)
    (loop [{:keys [state] :as cpu} (-> (clojure.edn/read-string (slurp (io/resource "day_13_no_floor.edn"))) 
                                       (intcode/new-computer)
                                       (add-quarters))
           output-buff []
           ticks 0]
      (case state
        :state/need-input (do (.refresh screen)
                              (recur (intcode/next-with-input cpu (get-input screen))
                                     output-buff
                                     (inc ticks)))
        :state/output (let [next-buff (conj output-buff (intcode/get-output cpu))
                            next-cpu (intcode/next-from-output cpu)]
                        (if (= 3 (count next-buff))
                          (do (apply put-tile screen next-buff)
                              (recur next-cpu
                                     []
                                     (inc ticks)))
                          (recur next-cpu
                                 next-buff
                                 (inc ticks))))
        :state/running (recur (intcode/next-state cpu)
                              output-buff
                              (inc ticks))
        :state/halted  (put-msg screen 45 2 "HALT!")))
    (.readInput screen)
    (prn "Last score: " @last-score)))

