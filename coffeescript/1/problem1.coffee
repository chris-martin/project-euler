exports.answer = ->
  sum = 0
  for i in [1..999]
    if (i % 3 == 0 || i % 5 == 0)
      sum += i
  sum
