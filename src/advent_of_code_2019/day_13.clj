(ns advent-of-code-2019.day-13
  (:require [advent-of-code-2019.intcode :as intcode]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as spec])
  (:import [com.googlecode.lanterna.terminal DefaultTerminalFactory]
           [java.io Closeable]
           [com.googlecode.lanterna
            SGR
            TextColor
            TextColor$ANSI
            TextCharacter]
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


(defprotocol GameIO
  (put-tile [_ x y tile])
  (get-input [_]))


(comment
  (def start (intcode/new-computer (intcode/read-program "day_13.txt")))
  (intcode/next-state (intcode/next-state start))
  


  )

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


(defn put-tile
  [screen x y tile]
  (if (= tile 4) (prn "paddle!" x y))
  (if (= [-1 0] [x y])
    (prn "score: " tile)
    (.setCharacter screen x y (gc tile) #_(get tile-char-mapping tile))))


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
