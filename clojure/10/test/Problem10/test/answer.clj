(ns Problem10.test.answer
  (:use [Problem10.answer :refer [answer]])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= 142913828922 (answer))))
