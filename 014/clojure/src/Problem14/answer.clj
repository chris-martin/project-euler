(ns Problem14.answer)

(defn collatz [i]
  (if (even? i)
    (/ i 2)
    (+ 1 (* 3 i))))

(defn get-lengths [lengths stack]
  (cond

    ; The stack is empty, we're done!
    (empty? stack) lengths

    ; The next item on the stack is already calculated, so just pop it off.
    (contains? lengths (first stack))
    (recur lengths (rest stack))

    ; We're looking at the tree edge x -> y, where length(x) is unknown
    :else
    (let [x (first stack)
          y (collatz x)
          y-length (get lengths y)]
      (if (not (nil? y-length))
        ; length(y) is already known,
        (recur (assoc lengths x (+ 1 y-length)) (rest stack))

        ; We haven't learned anything except that there's a new
        ; value y that we need to know; push y onto the stack.
        (recur lengths (cons y stack))))))

(defn key-with-max-value [map]
  (key (apply max-key val map)))

(def initial-lengths
  {1 1})

(def initial-stack
  (->> (range 1 1000001)))

(defn answer []
  (key-with-max-value
    (get-lengths initial-lengths initial-stack)))
