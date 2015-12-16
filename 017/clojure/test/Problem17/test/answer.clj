(ns Problem17.test.answer
  (:use [Problem17.answer :refer [answer]])
  (:use [clojure.test]))

(deftest answer-is-correct
  (is (= 21124 (answer))))
