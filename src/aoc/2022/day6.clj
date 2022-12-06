(ns aoc.2022.day6
  (:require [clojure.test :as t]))

(defn find-marker [size s]
  (->> (seq s)
       (partition size 1)
       (map (fn [offset data] [offset (set data)]) (iterate inc size))
       (filter (fn [[_ data]] (= size (count data))))
       ffirst))

(defn find-start-marker [s] (find-marker 4 s))
(defn find-message-marker [s] (find-marker 14 s))

(t/deftest day-5-test
  (t/are [x y] (= x y)
      5  (find-start-marker "bvwbjplbgvbhsrlpgdmjqwftvncz")
      6  (find-start-marker "nppdvjthqldpwncqszvftbrmjlhg")
      10 (find-start-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
      11 (find-start-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")

      19 (find-message-marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
      23 (find-message-marker "bvwbjplbgvbhsrlpgdmjqwftvncz")
      23 (find-message-marker "nppdvjthqldpwncqszvftbrmjlhg")
      29 (find-message-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
      26 (find-message-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")))

(comment
  (t/run-tests 'aoc.2022.day6)
  (find-start-marker (slurp "resource/2202-06.txt"))
  (find-message-marker (slurp "resource/2202-06.txt")))
