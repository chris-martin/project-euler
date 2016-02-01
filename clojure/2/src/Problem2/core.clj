(ns Problem2.core)

(def upperBound 4e6)

(defn bounded [xs]
  (take-while (fn [x] (< x upperBound)) xs))

(def fibs
  (concat '(0 1) (lazy-seq (map + fibs (rest fibs)))))

(defn answer []
  (reduce + (filter even? (bounded fibs))))
