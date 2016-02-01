Generator = require('./Generator.coffee').Generator;

exports.answer = ->
  return fibs()
    .takeWhile((x) -> x < 4000000)
    .filter((x) -> x % 2 == 0)
    .sum()

fibs = ->
  a = 0
  b = 1
  new Generator ->
    [a, b] = [b, a+b]
    a
