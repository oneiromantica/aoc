(ns aoc.aoc
  (:require [clojure.data.priority-map :as pm]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]))

(defn spy>> [msg val] (prn msg val) val)
(defn spy>  [val msg] (spy>> msg val))

(defn transpose [m]
  (apply mapv vector m))

(comment
  (transpose [[1 2 3]
              [4 5 6]
              [7 8 9]]))

(defn scanmap
  "((a,b) -> b) -> b -> [a] -> [b]"
  [f start coll]
  (reduce (fn [coll' nxt]
            (concat (if (seqable? coll') coll' (empty coll))
                    [(f nxt (if (seqable? start) (last coll') start))]))
          start
          coll))

(comment (= [2 2 2 3 4 5]
            (scanmap (fn [nxt prv] (if (> nxt prv) nxt prv))
                     2
                     (range 6))))

(defn a*
  "Shortest path.
    `h`: node -> node -> num     cost estimation to end
    `cost`: node -> node -> num  cost to reach neighbour
    `start`: node
    `end`: node"
  [{:keys [h cost start end] :or {cost (fn [a b] (->> (map - a b) (map #(Math/abs %)) (apply +)))
                                  h    (constantly 1)}}]
  (fn go
    ([nodes] (go (assoc-in nodes [start :g] 0)
                 (assoc (pm/priority-map-by
                         (fn [a b] {:pre [(not (nil? (:f a))) (not (nil? (:f b)))]}
                            (- (:f a) (:f b))))
                        start
                        (get nodes start))
                 #{}))
    ([nodes open closed]     
     (if-let [[curr] (peek open)]
       (if (= curr end)
        {:node curr, :state nodes}
        (let [border (set/difference (set (get-in nodes [curr :border] #{})) closed)
              nodes' (reduce (fn [state nxt]
                               (let [g      (get-in state [curr :g])
                                     g'     (+ g (cost curr nxt))
                                     h'     (h nxt end)
                                     f      (+ g h')
                                     better (< f (get-in state [nxt :g] Float/POSITIVE_INFINITY))
                                     new    (not (contains? open curr))
                                     nxt'   (assoc (get state nxt)
                                                   :f f
                                                   :g g'
                                                   :from curr)]
                                 (if (or new better)
                                   (assoc state nxt nxt')
                                   state)))
                             nodes
                             border)
              open'  (into (pop open) (map (fn [k] [k (get nodes k)]) border))]
          (recur nodes' open' (conj closed curr))))
       :no-path-found))))

(defn a*-backtrack
  ([{:keys [node] :as result}] (a*-backtrack result (list node)))
  ([{:keys [node state]} path]
   (if-let [origin (get-in state [node :from])]
     (recur {:state state, :node origin} (conj path origin))
     path)))

(comment
  (let [nodes {[0 0] {:border [[0 1] [1 0]]  :f 3}
               [0 1] {:border [[0 0] [1 1]]  :f 2}
               [0 2] {:border [[0 1] [1 2]]  :f 1}
               [1 0] {:border [[0 0] [1 1]]  :f 2}
               [1 1] {:border [[1 0] [1 2]]  :f 1}
               [1 2] {:border [[1 1] [0 2]]  :f 0}}
        manhattan (fn [a b] (->> (map - a b) (map #(Math/abs %)) (apply +)))
        find-path (a* {:h manhattan, :cost (constantly 1), :start [0 0], :end [1 2]})]
    ;; (pprint (find-path nodes))
    (a*-backtrack (find-path nodes))))
