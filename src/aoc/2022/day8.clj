(ns aoc.2022.day8
  (:require [aoc.aoc :refer [scanmap transpose]]
            [clojure.string :as str]))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map (partial re-seq #"\d"))
       (map (partial map parse-long))))
       

(defn visible-in-line [line]
  (scanmap (fn [nxt prv] (if (> (:h nxt) (get prv :max -1))
                          (-> (update nxt :v inc)
                              (assoc :max (:h nxt)))
                          (assoc nxt :max (:max prv))))
           nil
           line))

(defn visible-from-left [m]
  (pmap visible-in-line m))

(defn count-visible-in-line [line]
  (count (filter #(pos? (:v %)) line)))

(defn visible-from-outside [m]
  (->> m
       visible-from-left
       transpose
       visible-from-left
       (pmap reverse)
       visible-from-left
       (pmap reverse)
       transpose
       (pmap reverse)
       visible-from-left))

(defn part-1 [m]
  (->> (pmap (partial map (fn [h] {:h h, :v 0})) m)
       visible-from-outside
       (pmap count-visible-in-line)
       flatten
       (apply +)))

;;; part 2

(defn view-to-right [line]
  (map-indexed (fn [idx tree]
                 (let [trees-to-right (drop (inc idx) line)
                       smaller-trees  (take-while #(< % tree) trees-to-right)]
                   (min (count trees-to-right)
                        (inc (count smaller-trees)))))
               line))
(comment
    (view-to-right [3 3 5 4 9]))

(defn part-2 [input]
  (let [lt (flatten (pmap view-to-right input))
        rt (flatten (->> input (pmap reverse) (pmap view-to-right) (pmap reverse)))
        tp (flatten (->> input transpose (pmap view-to-right) transpose))
        bt (flatten (->> input
                         transpose (pmap reverse)
                         (pmap view-to-right)
                         (pmap reverse) transpose))]
    (apply max (pmap (fn [& ns] (apply * ns))
                     lt rt tp bt))))

(comment
    (-> (slurp "resource/2022-08.txt")
        parse-input
        ((juxt part-1 part-2))))
