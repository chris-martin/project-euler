(ns Problem19.answer)

(defn leap? [year] (cond (= 0 (rem year 400)) true
                         (= 0 (rem year 100)) false
                         (= 0 (rem year 4)) true
                         :else false))

(defn month-lengths [year] {1  31                           ; jan
                            2  (if (leap? year) 29 28)      ; feb
                            3  31                           ; mar
                            4  30                           ; apr
                            5  31                           ; may
                            6  30                           ; jun
                            7  31                           ; jul
                            8  31                           ; aug
                            9  30                           ; sep
                            10 31                           ; oct
                            11 30                           ; nov
                            12 31})                         ; dec

(defn days-in-month [month year] ((month-lengths year) month))

(defn incl "Inclusive range" [start end] (range start (+ 1 end)))

(def dates (for [year  (incl 1900 2000)
                 month (incl 1 12)
                 day   (incl 1 (days-in-month month year))]
             {:year  year
              :month month
              :day   day}))

(def dates-with-weekday (mapv #(assoc %1 :weekday %2)
                              dates
                              (cycle (incl 1 7))))

(defn matching-date? [date] (and (not= 1900 (date :year))
                                 (= 1 (date :day))
                                 (= 7 (date :weekday))))

(defn answer [] (count (filter matching-date? dates-with-weekday)))
