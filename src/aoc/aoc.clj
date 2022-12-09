(ns aoc.aoc)

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
