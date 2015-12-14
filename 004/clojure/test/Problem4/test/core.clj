(ns Problem4.test.core
  (:use [Problem4.core])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= 906609 (Problem4.core/answer))))
