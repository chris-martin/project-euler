(ns Problem1.test.core
  (:use [Problem1.core])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= 233168 (Problem1.core/answer))))
