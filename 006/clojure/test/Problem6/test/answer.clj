(ns Problem6.test.answer
  (:use [Problem6.answer :refer [answer]])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= 25164150 (answer))))
