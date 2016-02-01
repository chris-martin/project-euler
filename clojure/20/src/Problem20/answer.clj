(ns Problem20.answer)

(defn factorial [n]
  (reduce * (map bigint (range 1 (+ 1 n)))))

(defn char-to-int [c]
  (Integer/parseInt (str c)))

(defn digits [s]
  (map char-to-int (str s)))

(defn sum [xs]
  (reduce + xs))

(defn answer []
  (->> 100 factorial digits sum))
