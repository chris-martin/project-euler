exports.answer = ->
  x = 600851475143
  while true
    i = firstFactor x
    break if not i
    x /= i
  x

firstFactor = (x) ->
  for i in [2..x/2]
    return i if x % i == 0
