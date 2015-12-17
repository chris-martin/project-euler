(ns Problem19.test.answer
  (:use [Problem19.answer :refer [answer incl days-in-month]])
  (:use [clojure.test]))



(defn days-in-year [year]
  (reduce + (for [month (incl 1 12)]
              (days-in-month month year))))

(deftest days-in-1900
  "1900 isn't a leap year, 365 days as normal."
  (is (= 365 (days-in-year 1900))))

(deftest days-in-2000
  "2000 is a leap year, so there's an extra day."
  (is (= 366 (days-in-year 2000))))

(deftest answer-is-correct
  (is (= 171 (answer))))
