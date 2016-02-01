(ns Problem7.test.answer
  (:use [Problem7.answer :refer [answer]])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= 104743 (answer))))
