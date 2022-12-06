(ns aoc.2022.day1
  (:require [clojure.string :as str]
            [clojure.test :as t]))

(defn parse-input [s]
  (->> (str/split s #"\n\n")
       (map #(re-seq #"\d+" %))
       (map (fn [block] (map parse-long block)))))

(defn part-1 [input]
  (->> (map #(apply + %) input)
       (apply max)))

(defn part-2 [input]
  (->> (map #(apply + %) input)
       sort
       (take-last 3)
       (apply +)))

(t/deftest day-1-test
  (let [input "
    1000
    2000
    3000

    4000

    5000
    6000

    7000
    8000
    9000

    10000
    "]
    (t/is (= [[1000 2000 3000] [4000] [5000 6000] [7000 8000 9000] [10000]]
             (parse-input input)))
    (t/is (= 24000 (part-1 (parse-input input))))
    (t/is (= 45000 (part-2 (parse-input input))))))

(comment
  (t/run-tests 'aoc.2022.day1)
  (-> (slurp "resource/2022-01.txt")
      parse-input
      ((juxt part-1 part-2))))
