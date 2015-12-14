(ns Problem3.test.core
  (:use [Problem3.core])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= 6857 (Problem3.core/answer))))
