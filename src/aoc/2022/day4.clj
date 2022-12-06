(ns aoc.2022.day4)

(defn parse-input [s]
  (->> (re-seq #"\d+" s)
       (map parse-long)
       (partition 4 4)))

(defn to-sorted-tuples [[s1 e1 s2 e2]]
  (sort-by (juxt first (comp - second)) [[s1 e1] [s2 e2]]))

(defn enclosed? [[[a1 a2] [b1 b2]]]
  (<= a1 b1 b2 a2))

(defn overlaps? [[[a1 a2] [b1 _]]]
  (<= a1 b1 a2))

(defn part-1 [input]
  (->> (map to-sorted-tuples input)
       (filter enclosed?)
       count))

(defn part-2 [input]
  (->> (map to-sorted-tuples input)
       (filter overlaps?)
       count))

(comment
  (let [input "
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
"]
    (parse-input input)
    (part-2 (parse-input input)))
  (->> (slurp "resource/2022-04.txt")
       parse-input
       ((juxt part-1 part-2))))
