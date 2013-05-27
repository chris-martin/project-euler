def answer()
  (1..999).
    select { |n| [3,5].any? { |d| n % d == 0 } }.
    inject(:+)
end