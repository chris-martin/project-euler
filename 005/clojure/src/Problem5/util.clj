(ns Problem5.util)

(defn product [& xs]
  (reduce * 1 xs))

(defn key-set [map]
  (set (keys map)))

(defn divides? [x y]
  (= 0 (rem y x)))
