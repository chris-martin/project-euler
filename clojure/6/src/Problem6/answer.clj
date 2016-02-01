(ns Problem6.answer)

(defn sum [xs]
  (reduce + 0 xs))

(defn square [n]
  (* n n))

(defn sum-of-squares [r]
  (sum (map square r)))

(defn square-of-sum [r]
  (square (sum r)))

(defn answer []
  (let [r (range 1 101)]
    (- (square-of-sum r) (sum-of-squares r))))
