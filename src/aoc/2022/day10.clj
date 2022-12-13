(ns aoc.2022.day10
  (:require [clojure.string :as str]))

(defn parse-input [s]
  (->> 
       (map (fn [line]
              (if-let [[_ n-str] (re-find #"addx (-?\d+)" line)]
                {:add (parse-long n-str), :noop false}
                {:noop true}))
            (str/split-lines s))))

(defn step [history instr]
  (let [state (last history)]
    (cond
      (:noop instr) (conj history (update (into state instr) :cycle inc))
      :else         (-> history
                      (conj (update (into state instr) :cycle inc)
                        (conj (-> state (into instr) (update :x + (:add instr)) (update :cycle + 2))))))))

(defn exec-program [input]
  (reduce step [{:x 1, :cycle 1}] input))

(defn part-1 [input]
  (->> input
       exec-program
       (filter (fn [{:keys [cycle]}] (some #{cycle} [20 60 100 140 180 220])))
       (map (fn [{:keys [cycle x]}] (* cycle x)))
       (apply +)))

(defn render [line]
  (map (fn [{:keys [x cycle]}]
         (let [i (rem (dec cycle) 40)]
           (if (<= (dec i) x (inc i)) \# \.)))
       line))

(defn part-2 [input]
  (->> (exec-program input)
       render
       (partition 40 40)
       (map #(str/join %))
       (str/join \newline)))

(comment
  (let [input (parse-input (slurp "resource/2022-10.txt"))]
    (prn (part-1 input))
    (println (part-2 input))))
