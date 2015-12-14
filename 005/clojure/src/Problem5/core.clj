(ns Problem5.core
  (:require [clojure.set]))

; generic utils

(defn product [& xs]
  (reduce * 1 xs))

(defn key-set [map]
  (set (keys map)))

(defn divides? [x y]
  (= 0 (rem y x)))

; bags are represented as map from item to cardinality

(def bag-empty "An empty bag" {})

(defn bag-cardinality "How many elem are in the bag?" [bag elem]
  (bag elem 0))

(defn bag-add "Add one elem to the bag" [bag elem]
  (assoc bag elem (+ 1 (bag-cardinality bag elem))))

(defn bag-union "Union of any number of bags" [& bags]
  (into {} (for [elem (apply clojure.set/union (map key-set bags))]
             (let [cardinality (apply max (for [bag bags]
                                            (bag-cardinality bag elem)))]
               [elem cardinality]))))

(defn bag->list [bag]
  (for [[elem cardinality] (into [] bag)
        x (repeat cardinality elem)]
    x))

; primes

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

; answer

(defn ^long answer []
  (apply product (bag->list (apply bag-union (map factorize (range 2 20))))))
