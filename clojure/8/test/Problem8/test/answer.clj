(ns Problem8.test.answer
  (:use [Problem8.answer :refer [answer]])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= 40824 (answer))))
