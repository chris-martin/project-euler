(ns Problem5.bag
  "A bags (or multiset) is represented as map from elem to cardinality."
  (:require [clojure.set]
            [Problem5.util :refer [key-set]]))

(def bag-empty "An empty bag" {})

(defn bag-cardinality "How many elem are in the bag?" [bag elem]
  (bag elem 0))

(defn bag-add "Add one elem to the bag" [bag elem]
  (assoc bag elem (+ 1 (bag-cardinality bag elem))))

(defn bag-union "Union of any number of bags" [& bags]
  (into {} (for [elem (apply clojure.set/union (map key-set bags))]
             (let [cardinality (apply max (for [bag bags]
                                            (bag-cardinality bag elem)))]
               [elem cardinality]))))

(defn bag->list [bag]
  (for [[elem bag-cardinality] (into [] bag)
        x (repeat bag-cardinality elem)]
    x))
