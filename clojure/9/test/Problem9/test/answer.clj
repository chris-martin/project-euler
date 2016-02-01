(ns Problem9.test.answer
  (:use [Problem9.answer :refer [answer]])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= 31875000 (answer))))
