(ns Problem14.answer
  (:require [clojure.core.match :refer [match]]))

(defn collatz [i]
  (if (even? i)
    (/ i 2)
    (+ 1 (* 3 i))))

(defn get-lengths [lengths stack]
  (match [stack]

         ; The stack is empty, we're done!
         [([] :seq)] lengths

         ; The next item on the stack is already calculated,
         [([(known-value :guard #(contains? lengths %)) & new-stack] :seq)]
         (recur lengths new-stack)

         ; We're looking at the tree edge x -> y, where length(x) is unknown
         [([x & rest-of-stack] :seq)]
         (let [y (collatz x)]
           ; See if we already know length(y)
           (match [(get lengths y)]

                  ; length(y) is already known,
                  [(y-length :guard (complement nil?))]
                  (recur (assoc lengths x (+ 1 y-length)) rest-of-stack)

                  ; We haven't learned anything except that there's a new
                  ; value y that we need to know;
                  [nil]
                  ; push y onto the stack.
                  (recur lengths (cons y stack))))))

(defn key-with-max-value [map]
  (key (apply max-key val map)))

(def initial-lengths
  {1 1})

(def initial-stack
  (->> (range 1 1000001)))

(defn answer []
  (key-with-max-value (get-lengths initial-lengths initial-stack)))
