(ns Problem20.test.answer
  (:use [Problem20.answer :refer [answer]])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= 648 (answer))))
