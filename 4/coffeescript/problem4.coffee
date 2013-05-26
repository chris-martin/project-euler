Generator = require('./Generator.coffee').Generator

exports.answer = ->
  Generator.range(1, 999)
    .flatMap (x) ->
      Generator.range(1, 999).map (y) ->
        x * y
    .filter(isPalindrome)
    .max()

isPalindrome = (x) ->
  s = x.toString()
  s == s.split('').reverse().join('')
