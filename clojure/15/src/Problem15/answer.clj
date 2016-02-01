(ns Problem15.answer)

(defn count-paths [counts stack]
  (let [xy (first stack)
        [x y] xy
        adjacencies (if (nil? xy) nil [[(- x 1) y]
                                       [x (- y 1)]])
        unknown-adjacencies (filter #(not (contains? counts %)) adjacencies)]
    (cond

      ; When the stack is empty, we're done.
      (empty? stack) counts

      ; If the stack head's count is already known, just pop it off.
      (contains? counts xy) (recur counts (rest stack))

      ; If either coordinate is zero, there's only one path.
      ; Add it to the counts and pop it off the stack,
      (some #{0} xy) (recur (assoc counts xy 1) (rest stack))

      ; If both of the adjacencies' counts are known, then this count
      ; is their sum. Add it to the counts and pop it off the stack,
      (empty? unknown-adjacencies)
      (let [count (apply + (map (partial get counts) adjacencies))]
        (recur (assoc counts xy count) (rest stack)))

      ; There are some unknown adjacencies. Add them to the stack.
      :else (recur counts (concat unknown-adjacencies stack)))))

(defn answer []
  (let [d [20 20]]
    (get (count-paths {} [d]) d)))
