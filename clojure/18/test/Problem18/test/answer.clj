(ns Problem18.test.answer
  (:use [Problem18.answer :refer [answer reduce-row reduce-triangle]])
  (:use [clojure.test]))

(deftest test-reduce-row
  (is [4 7 9]
      (reduce-row [1 2 4] [2 3 5 1])))

(deftest test-reduce-triangle
  (is [[1] [2 3] [4 7 9]]
      (reduce-triangle [[1] [2 3] [1 2 4] [2 3 5 1]])))

(deftest answer-is-correct
  (is (= 1074 (answer))))
