(ns aoc.2022.day5
  (:require [clojure.string :as str]
            [clojure.test :as t]))

(defn parse-instructions [input-seq]
  (->> input-seq
       (map (partial re-seq #"\d+"))
       (map #(map parse-long %))))

(defn parse-stack-line [line]
  (->> line
       (re-seq #"    ?|.\w. ?")
       (map (partial re-seq #"\w"))
       (map ffirst)))

(defn into-stacks [stacks items]
  (reduce (fn [stack [item idx]]
            (if item (update stack idx conj item)
                stack))
          stacks
          (map (fn [& args] args) items (range))))

(defn parse-stacks
  ([input-seq] (let [n ((comp count parse-stack-line first) input-seq)
                     stacks (vec (take n (cycle ['()])))]
                 (parse-stacks input-seq stacks)))
  ([[head & tail] stacks]
   (if head
     (recur tail
            (into-stacks stacks
                         (parse-stack-line head)))
     (mapv reverse stacks))))

(defn move [stacks [n from to]]
  (if (zero? n) stacks
      (let [from-idx (dec from), to-idx (dec to)]
        (recur (-> stacks
                   (update from-idx rest)
                   (update to-idx #(conj % (first (get stacks from-idx)))))
               [(dec n) from to]))))

(defn move-9001 [stacks [n from to]]
  (let [from-idx (dec from), to-idx (dec to)]
    (-> stacks
        (update from-idx #(drop n %))
        (update to-idx #(concat (take n (get stacks from-idx)) %)))))

(defn parse-input [s]
  (let [[stacks-str instr-str] (str/split s #"\n\n")]
    (prn "stacks-str" stacks-str)
    {:stacks (parse-stacks (butlast (str/split-lines stacks-str)))
     :instructions (parse-instructions (str/split-lines instr-str))}))

(defn part-1 [{:keys [stacks instructions]}]
  (clojure.string/join (map first (reduce move stacks instructions))))

(defn part-2 [{:keys [stacks instructions]}]
  (clojure.string/join (map first (reduce move-9001 stacks instructions))))

(t/deftest day-5
  (let [input-str
        (clojure.string/join
         \newline
         ["    [D]    "
          "[N] [C]    "
          "[Z] [M] [P]"
          " 1   2   3 "
          ""
          "move 1 from 2 to 1"
          "move 3 from 1 to 3"
          "move 2 from 2 to 1"
          "move 1 from 1 to 2"])]
    (t/is (= [\N \C nil] (parse-stack-line "[N] [C]    ")))
    (t/is (= ['() '(2 1)] (into-stacks ['() '(1)] [nil 2])))
    (t/is (= [[\N \Z] [\D \C \M] [\P]]
             (parse-stacks ["    [D]    ", "[N] [C]    ", "[Z] [M] [P]"])))
    (t/is (= [[\C \D \N \Z] [\M] [\P]] (move ['(\N \Z) '(\D \C \M) '(\P)] [2 2 1])))
    (t/is (= "CMZ" (-> input-str parse-input part-1)))
    (t/is (= "MCD" (-> input-str parse-input part-2)))))

(comment
  (t/run-tests 'aoc.2022.day5)
  (-> (slurp "resource/2022-05.txt") parse-input part-1 prn)
  (-> (slurp "resource/2022-05.txt") parse-input part-2 prn))
