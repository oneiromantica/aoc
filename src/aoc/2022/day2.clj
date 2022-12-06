(ns aoc.2022.day2
  (:require [clojure.test :as t]))

(defn parse-input [s]
  (partition 2 2 (re-seq #"\w" s)))

(defn shape-score [shape]
  (get {"A" 1, "B" 2, "C" 3, "X" 1, "Y" 2, "Z" 3} shape 0))

(defn outcome-score [a b]
  (if (= a b) 3
      (get {["C" "X"] 6, ["A" "Y"] 6, ["B" "Z"] 6
            ["A" "X"] 3, ["B" "Y"] 3, ["C" "Z"] 3}
           [a b] 0)))

;; X lose, Y draw, Z win
(defn score-2 [[a b]]
  (get {["A" "X"] (shape-score "C")
        ["B" "X"] (shape-score "A")
        ["C" "X"] (shape-score "B")
        ["A" "Y"] (+ 3 (shape-score "A"))
        ["B" "Y"] (+ 3 (shape-score "B"))
        ["C" "Y"] (+ 3 (shape-score "C"))
        ["A" "Z"] (+ 6 (shape-score "B"))
        ["B" "Z"] (+ 6 (shape-score "C"))
        ["C" "Z"] (+ 6 (shape-score "A"))}
       [a b]))

(defn part-1 [input]
  (->> input
       (map (fn [[a b]] (+ (shape-score b) (outcome-score a b))))
       (apply +)))

(defn part-2 [input]
  (apply + (map score-2 input)))

(t/deftest day-2-tests
  (let [input "A Y\nB X\nC Z"]
    (t/is (= [["A" "Y"] ["B" "X"] ["C" "Z"]]
             (parse-input input)))
    (t/is (= 15 (part-1 (parse-input input))))
    (t/is (= 12 (part-2 (parse-input input))))))
(t/deftest scores
  (t/is (= [1 2 3] (map shape-score ["A" "B" "C"])))
  (t/is (= 6 (outcome-score "A" "Y")))
  (t/is (= 0 (outcome-score "B" "X")))
  (t/is (= 3 (outcome-score "C" "Z")))
  (t/is (= 4 (score-2 ["A" "Y"])))
  (t/is (= 1 (score-2 ["B" "X"]))))

(comment
  (t/run-tests 'aoc.2022.day2)
  ((juxt part-1 part-2) (parse-input (slurp "resource/2022-02.txt"))))
