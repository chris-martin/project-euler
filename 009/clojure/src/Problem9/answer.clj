(ns Problem9.answer)

(defn square [n]
  (* n n))

(defn pythag? [a b c]
  (= (+ (square a) (square b))
     (square c)))

(defn answer []
  (first (for [a (range 1000)
               b (range a)
               c [(- 1000 a b)]
               :when (pythag? a b c)]
           (* a b c))))
