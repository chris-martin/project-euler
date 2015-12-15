(ns Problem11.test.answer
  (:use [Problem11.answer :refer [answer]])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= 70600674 (answer))))
