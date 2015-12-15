(ns Problem12.test.answer
  (:use [Problem12.answer :refer [answer]])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= 76576500 (answer))))
