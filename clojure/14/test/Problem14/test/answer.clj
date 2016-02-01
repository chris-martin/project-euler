(ns Problem14.test.answer
  (:use [Problem14.answer :refer [answer]])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= 837799 (answer))))
