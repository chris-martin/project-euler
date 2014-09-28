(ns Problem1.core)

(defn dividedBy? [n d]
  (= 0 (mod n d)))

(defn isMultiple? [n]
  (some (partial dividedBy? n) '(3 5)))

(defn answer []
  (reduce + (filter isMultiple? (range 1 1000))))
