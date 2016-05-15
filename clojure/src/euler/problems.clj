(ns euler.problems
    (:require [clojure.core.match :refer [match]]))

(defn
  answer [problem-number]
  (match
    problem-number

    1 (let [dividedBy? (fn [n d] (= 0 (mod n d)))
            isMultiple? (fn [n] (some (partial dividedBy? n) '(3 5)))]
           (reduce + (filter isMultiple? (range 1 1000))))

    ))
