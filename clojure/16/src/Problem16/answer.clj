(ns Problem16.answer)

(defn exp [x n]
  (.pow (java.math.BigInteger/valueOf x) n))

(defn char-to-int [c]
  (Integer/parseInt (str c)))

(defn digits [i]
  (map char-to-int (str i)))

(defn sum [xs]
  (reduce + xs))

(defn answer []
  (sum (digits (exp 2 1000))))
