(ns Problem15.test.answer
  (:use [Problem15.answer :refer [answer]])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= 137846528820 (answer))))
