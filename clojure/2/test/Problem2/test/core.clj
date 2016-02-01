(ns Problem2.test.core
  (:use [Problem2.core])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= 4613732 (Problem2.core/answer))))
