(ns Problem10.answer)

(defn ^boolean prime? [^long n]
  (.isProbablePrime (BigInteger/valueOf n) 10))

(def primes (filter prime? (range)))

(defn answer []
  (reduce + (take-while #(< % 2000000) primes)))
