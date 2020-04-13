(ns advent-of-code-2019.day-13
  (:require [advent-of-code-2019.intcode :as intcode]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]
            [clojure.java.io :as io]
            [clojure.core.async :as async])
  (:import [com.googlecode.lanterna.terminal DefaultTerminalFactory]
           [java.io Closeable]
           [com.googlecode.lanterna
            SGR
            TextColor
            TextColor$ANSI
            TextCharacter
            TerminalSize]
           [com.googlecode.lanterna.input KeyStroke KeyType]
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

(defn gc
  [t]
  (if (zero? t) 
    (lchar \space TextColor$ANSI/BLACK)
    (lchar \# TextColor$ANSI/GREEN)
    )
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


; init game
;  keyboard event handlers -> joystick register, 0 neutral, -1 left, 1 right
;  score display
;  42x25 blocks display


(defn -main
  [& args]
  (let [term (.createTerminal (DefaultTerminalFactory.))
        screen (TerminalScreen. term)]
    (try
      (.startScreen screen)
      (.readInput screen)
      (.doResizeIfNecessary screen)
      (prn (.getTerminalSize screen))
      (loop [{:keys [state] :as cpu} (add-quarters (intcode/new-computer (intcode/read-program "day_13.txt")))
             output-buff []]
        (case state
          :state/need-input (do (.refresh screen)
                                (Thread/sleep 500)
                                (recur (intcode/resume-with-input cpu 0) 
                                       output-buff)) 
          :state/output (let [next-buff (conj output-buff (intcode/get-output cpu))
                              next-cpu (intcode/resume-from-output cpu)]
                          (if (= 3 (count next-buff))
                            (do (apply put-tile screen next-buff) 
                                (recur next-cpu []))
                            (recur next-cpu next-buff)))
          :state/halted (do (prn "halt!") (.readInput screen)) 
          :state/running (recur (intcode/next-state cpu)
                                output-buff)))
      (finally
        (.stopScreen screen)
        (.close term)))))


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


(comment
  (def prgs (->> (slurp "cpu2")
                 (clojure.string/split-lines)
                 (map #(clojure.edn/read-string %))
                 (map :program)
                 (map vec)))


  (doseq [[xs ys] (partition 2 1 prgs)]
    (prn (diff-states xs ys))
    )

  (defn diff-states
    [xs ys]
    (reduce (fn [diffs i]
              (let [x (get xs i)
                    y (get ys i)]
                (if (= x y)
                  diffs
                  (conj diffs [i x y]))))
            []
            (range (max (count xs) (count ys)))))
  

  )



(comment

  (def term (.createTerminal (doto (DefaultTerminalFactory.)
                               (.setInitialTerminalSize (TerminalSize. 42 25)))))
  (type term)

  (.bell term)
  (.getTerminalSize term)
  (.close term)

  (defn get-key
    [screen]
    (if-let [s (.pollInput screen)]
      (.getKeyType s)))


  (with-open [wr (io/writer "prog.edn")]
    (clojure.pprint/pprint
      (intcode/read-program "day_13.txt")
      wr))



  (with-screen
    [screen 80 40]
    (.readInput screen)
    (loop [{:keys [state] :as cpu} (-> (clojure.edn/read-string (slurp "prog.edn")) 
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
    (.readInput screen))


  (def prgs (->> (slurp "cpu2")
                 (clojure.string/split-lines)
                 (map #(clojure.edn/read-string %))
                 (map :program)))


  (doseq [[xs ys] (partition 2 1 prgs)]
    (prn (diff-states xs ys))
    )

  


  (defn diff
    [xs ys]
    (assert (= (count xs) (count ys)))
    (->> (map (fn [i x y] [i (= x y)]) (range (count xs)) xs ys)
         (filter (fn [[i eq?]] (not eq?)))
         ))


  (diff (first prgs)
        (second prgs)
        )


  (map (fn [[xs ys]] [(count xs) (count ys)]) (partition 2 1 prgs))


  (require 'clojure.data)

  (doseq [[prev state] (partition 2 1 prgs)]
    (prn (second (clojure.data/diff prev state)))
    )



  ; score = 21415


  ;

  

  
 
    


  )
