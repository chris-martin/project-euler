(ns Problem5.test.answer
  (:use [Problem5.answer :refer [answer]])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= 232792560 (answer))))
