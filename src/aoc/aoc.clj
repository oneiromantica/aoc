(ns aoc.aoc)

(defn spy>> [msg val] (prn msg val) val)
(defn spy>  [val msg] (spy>> msg val))
