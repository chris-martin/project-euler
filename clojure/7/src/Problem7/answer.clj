(ns Problem7.answer)

(defn ^boolean prime? [^long n]
  (.isProbablePrime (BigInteger/valueOf n) 10))

(def primes (filter prime? (range)))

(defn answer []
  (first (drop 10000 primes)))
