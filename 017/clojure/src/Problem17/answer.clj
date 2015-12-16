(ns Problem17.answer)

(def _1-9
  ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])

(def _10-19
  ["ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen"
   "sixteen" "seventeen" "eighteen" "nineteen"])

(def _20-99
  (for [a ["twenty" "thirty" "forty" "fifty"
           "sixty" "seventy" "eighty" "ninety"]
        b (cons "" _1-9)]
    (str a b)))

(def _1-99
  (concat _1-9 _10-19 _20-99))

(def _100-999
  (for [a _1-9
        b (cons "" (map #(str "and" %) _1-99))]
    (str a "hundred" b)))

(def _1-1000
  (concat _1-99 _100-999 ["onethousand"]))

(defn answer []
  (->> _1-1000 (map count) (reduce +)))
