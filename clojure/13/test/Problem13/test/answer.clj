(ns Problem13.test.answer
  (:use [Problem13.answer :refer [answer]])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= "5537376230" (answer))))
