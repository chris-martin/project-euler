(ns euler_test.problems
  (:use [euler.problems :refer [answer]])
  (:use [clojure.test]))

(map (fn [n] (deftest (symbol (str "problem " n)) (= (answer n) 45))) [1 2 3])
