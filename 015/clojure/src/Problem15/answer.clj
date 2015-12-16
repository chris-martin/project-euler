(ns Problem15.answer
  (:require [clojure.core.match :refer [match]]))

(defn right [dim] (let [[x y] dim] [(- x 1) y]))
(defn down [dim] (let [[x y] dim] [x (- y 1)]))

(defn count-paths [counts stack]
  (cond (empty? stack) counts
        (contains? counts (first stack)) (recur counts (rest stack))
        :else (let [dim (first stack)]
                (match [dim
                        [(contains? counts (right dim))
                         (contains? counts (down dim))]]
                       [[0 y] _] (recur (assoc counts dim 1) (rest stack))
                       [[x 0] _] (recur (assoc counts dim 1) (rest stack))
                       [_ [true true]] (recur (assoc counts dim (+ (get counts (right dim))
                                                                   (get counts (down dim))))
                                              (rest stack))
                       [_ [true false]] (recur counts (->> stack
                                                           (cons (down dim))))
                       [_ [false true]] (recur counts (->> stack
                                                           (cons (right dim))))
                       [_ [false false]] (recur counts (->> stack
                                                            (cons (down dim))
                                                            (cons (right dim))))))))

(defn answer []
  (let [d [20 20]]
    (get (count-paths {} [d]) d)))
