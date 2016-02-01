(ns Problem3.core)

(defn prime? [n] (.isProbablePrime (BigInteger/valueOf n) 10))

(def primes (filter prime? (range)))

(defn divides? [x y]
  (= 0 (rem y x)))

(defn smallest-factor [n]
  (first (filter #(divides? % n) primes)))

(defn largest-factor [n]
  (if (prime? n)
    n
    (recur (/ n (smallest-factor n)))))

(defn answer []
  (largest-factor 600851475143))
