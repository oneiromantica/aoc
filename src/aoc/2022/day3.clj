(ns aoc.2022.day3
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input [s]
  (remove nil? (map seq (str/split-lines s))))

(defn to-compartments [items]
  (let [n (count items)
        size (/ n 2)]
    (->> ((juxt #(take size %) #(drop size %)) items)
         (map set)
         ((fn [[left right]] {:left left
                              :right right
                              :both (set/intersection left right)})))))

(defn to-score [char]
  (let [int-val (int char), int-a (int \a), int-z (int \z)]
    (if (<= int-a int-val int-z)
      (inc (- int-val int-a))
      (+ 2 (- int-z int-a)
         (- int-val (int \A))))))

(defn part-1 [input]
  (->> (map to-compartments input)
       (map (comp to-score first :both))
       (apply +)))

(defn part-2 [input]
  (->> (partition 3 3 input)
       (map #(map set %))
       (map #(apply set/intersection %))
       (map (comp to-score first))
       (apply +)))

(comment
  (to-compartments "vJrwpWtwJgWrhcsFMMfFFhFp")
  (map (fn [x] {x (to-score x)}) (seq "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))

(comment
  (let [tst "
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"]
    (part-1 (parse-input tst))
    (prn "part 2" (part-2 (parse-input tst)))
    (-> (slurp "resource/2022-03.txt")
        parse-input
        part-2)))
