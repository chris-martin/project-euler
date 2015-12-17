(ns Problem18.answer
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [line]
  (map #(Integer/parseInt %)
       (str/split (.trim line) #"\s+")))

(defn get-triangle []
  (with-open [rdr (io/reader (io/resource "triangle.txt"))]
    (->> rdr
         (line-seq)
         (filter (complement str/blank?))
         (map parse-line)
         doall)))

(defn reduce-row [xs ys]
  (mapv (fn [x y1 y2] (+ x (max y1 y2)))
        xs ys (drop 1 ys)))

; collapses the last two rows of the triangle
(defn reduce-triangle [rows]
  (concat (drop-last 2 rows)
          [(apply reduce-row (take-last 2 rows))]))

(defn answer []
  (->> (get-triangle)
       (iterate reduce-triangle)
       (filter #(= 1 (count %)))
       first
       first
       first))
