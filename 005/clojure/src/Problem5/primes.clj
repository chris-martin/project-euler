(ns Problem5.primes
  (:require [Problem5.bag :refer [bag-add bag-empty]]
            [Problem5.util :refer [divides?]]))

(defn ^boolean prime? [^long n]
  (.isProbablePrime (BigInteger/valueOf n) 10))

(def primes (filter prime? (range)))

(defn smallest-factor [n]
  (first (filter #(divides? % n) primes)))

(defn factorize "Get a bag of prime factors"
  ([n] (factorize n bag-empty))
  ([n agg] (if (= 1 n)
             agg
             (let [f (smallest-factor n)]
               (recur (/ n f) (bag-add agg f))))))
