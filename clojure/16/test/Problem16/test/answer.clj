(ns Problem16.test.answer
  (:use [Problem16.answer :refer [answer]])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= 1366 (answer))))
