(ns Problem12.answer)

(defn ^boolean prime? [^long n]
  (.isProbablePrime (BigInteger/valueOf n) 20))

(def primes
  (filter prime? (range)))

(def triangles
  (reductions + (drop 1 (range))))

(defn product [xs]
  (reduce * 1 xs))

(defn smallest-factor [n]
  (->> primes
       (filter #(= 0 (rem n %)))
       (first)))

(defn bag-add [bag elem]
  (assoc bag elem (+ 1 (get bag elem 0))))

(defn number-of-factors
  ([n] (number-of-factors n {}))
  ([n factor-bag] (if (= n 1)
                    (product (map (partial + 1) (vals factor-bag)))
                    (let [p (smallest-factor n)]
                      (recur (/ n p)
                             (bag-add factor-bag p))))))

(defn answer []
  (->> triangles
       (filter #(> (number-of-factors %) 500))
       (first)))
