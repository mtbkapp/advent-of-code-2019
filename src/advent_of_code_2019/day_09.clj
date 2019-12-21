(ns advent-of-code-2019.day-09
  (:require [advent-of-code-2019.intcode :as intcode]))



(defn run-program
  [input]
  (-> (intcode/read-program "day_09.txt")
      (intcode/new-computer)
      (intcode/run)
      (intcode/resume-with-input input)
      (intcode/get-output)))


; part 1
#_(run-program 1)


; part 2, ~15s, slow... 
#_(time (run-program 2))
