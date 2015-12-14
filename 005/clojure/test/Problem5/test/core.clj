(ns Problem5.test.core
  (:use [Problem5.core])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= 232792560 (Problem5.core/answer))))
