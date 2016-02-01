(ns Problem4.core)

(defn ^boolean palindrome-str? [^String s]
  (= s (clojure.string/reverse s)))

(defn ^boolean palindrome-int? [^long n]
  (palindrome-str? (.toString n)))

(defn products []
  (for [i (range 100 999)
        j (range 100 i)]
    (* i j)))

(defn ^long answer []
  (apply max (filter palindrome-int? (products))))
