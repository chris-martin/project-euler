(ns Problem5.answer
  (:require [Problem5.util :refer [product]]
            [Problem5.bag :refer [bag->list bag-union]]
            [Problem5.primes :refer [factorize]]))

(defn ^long answer []
  (apply product (bag->list (apply bag-union (map factorize (range 2 20))))))
