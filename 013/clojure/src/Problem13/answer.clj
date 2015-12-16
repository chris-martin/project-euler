(ns Problem13.answer
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-numbers []
  (with-open [rdr (io/reader (io/resource "numbers.txt"))]
    (->> rdr
         (line-seq)
         (filter (complement str/blank?))
         (map bigint)
         doall)))

(defn first-chars [n s]
  (subs s 0 n))

(defn answer []
  (->> (get-numbers)
       (reduce +)
       str
       (first-chars 10)))
